use std::str::FromStr;

use tower_lsp::lsp_types;
use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};
use tree_sitter::TreeCursor;

#[non_exhaustive]
#[derive(PartialEq)]
enum Kind {
    ContractDeclaration,
    InterfaceDeclaration,
    ErrorDeclaration,
    LibraryDeclaration,
    StructDeclaration,
    EnumDeclaration,
    FunctionDefinition,
    ConstantVariableDeclaration,
    UserDefinedTypeDefinition,
    EventDefinition,
    SourceFile,
    ContractBody,
}

struct KindToEnumFailure;
impl std::str::FromStr for Kind {
    type Err = KindToEnumFailure;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "contract_declaration" => Ok(Kind::ContractDeclaration),
            "interface_declaration" => Ok(Kind::InterfaceDeclaration),
            "error_declaration" => Ok(Kind::ErrorDeclaration),
            "library_declaration" => Ok(Kind::LibraryDeclaration),
            "struct_declaration" => Ok(Kind::StructDeclaration),
            "enum_declaration" => Ok(Kind::EnumDeclaration),
            "function_definition" => Ok(Kind::FunctionDefinition),
            "contract_variable_declaration" => Ok(Kind::ConstantVariableDeclaration),
            "user_defined_type_definition" => Ok(Kind::UserDefinedTypeDefinition),
            "event_definition" => Ok(Kind::EventDefinition),
            "source_file" => Ok(Kind::SourceFile),
            "contract_body" => Ok(Kind::ContractBody),
            _ => Err(KindToEnumFailure),
        }
    }
}

#[derive(Debug)]
struct NotImplementedError;
impl Kind {
    fn lsp_kind(&self) -> Result<SymbolKind, NotImplementedError> {
        match self {
            Kind::ContractDeclaration => Ok(SymbolKind::CLASS),
            Kind::InterfaceDeclaration => Ok(SymbolKind::INTERFACE),
            Kind::ErrorDeclaration => Ok(SymbolKind::OBJECT), // TODO: maybe theres a better option
            Kind::LibraryDeclaration => Ok(SymbolKind::CLASS),
            Kind::StructDeclaration => Ok(SymbolKind::STRUCT),
            Kind::EnumDeclaration => Ok(SymbolKind::ENUM),
            Kind::FunctionDefinition => Ok(SymbolKind::FUNCTION),
            Kind::ConstantVariableDeclaration => Ok(SymbolKind::CONSTANT),
            Kind::UserDefinedTypeDefinition => Ok(SymbolKind::OBJECT), // TODO: maybe there's a better option
            Kind::EventDefinition => Ok(SymbolKind::EVENT),
            Kind::SourceFile | Kind::ContractBody => Err(NotImplementedError),
        }
    }

    fn name(&self, node: &tree_sitter::Node, src: &[u8]) -> String {
        match self {
            Kind::ContractDeclaration => get_node_field(node, src, "name", "contract_declaration"),
            Kind::InterfaceDeclaration => {
                get_node_field(node, src, "name", "interface_declaration")
            }
            Kind::ErrorDeclaration => get_node_field(node, src, "name", "error_declaration"),
            Kind::LibraryDeclaration => get_node_field(node, src, "name", "library_declaration"),
            Kind::StructDeclaration => get_node_field(node, src, "name", "struct_declaration"),
            Kind::EnumDeclaration => get_node_field(node, src, "name", "enum_declaration"),
            Kind::FunctionDefinition => get_node_field(node, src, "name", "function_definition"),
            Kind::ConstantVariableDeclaration => {
                get_node_field(node, src, "name", "constant_variable_declaration")
            }
            Kind::UserDefinedTypeDefinition => {
                get_node_field(node, src, "name", "user_defined_type_definition")
            }
            Kind::EventDefinition => get_node_field(node, src, "name", "event_definition"),
            Kind::ContractBody | Kind::SourceFile => "".to_string(),
        }
    }
}

fn get_node_field(
    node: &tree_sitter::Node,
    src: &[u8],
    field_name: &str,
    node_name: &str,
) -> String {
    node.child_by_field_name(field_name)
        .expect(format!("field name not found on {}", node_name).as_str())
        .utf8_text(src)
        .expect("error getting text for node")
        .to_string()
}

fn lsp_position(point: tree_sitter::Point) -> lsp_types::Position {
    lsp_types::Position {
        line: point.row as u32,
        character: point.column as u32,
    }
}

fn lsp_range(node: &tree_sitter::Node) -> lsp_types::Range {
    let range = node.range();
    lsp_types::Range {
        start: lsp_position(range.start_point),
        end: lsp_position(range.end_point),
    }
}

fn get_document_symbol(cursor: &mut TreeCursor, src: &[u8]) -> Option<Vec<DocumentSymbol>> {
    let node = cursor.node();
    Kind::from_str(node.kind()).map_or(None, |kind| {
        if kind == Kind::ContractBody || kind == Kind::SourceFile {
            return Some(get_document_symbols(cursor, src));
        }

        let has_children = cursor.goto_first_child();
        tracing::debug!("has_children: {}", has_children);
        let child_symbols = if has_children {
            get_document_symbols(cursor, src)
        } else {
            vec![]
        };
        // if it had children, go back to parent to get back to same position as earlier
        if has_children {
            tracing::debug!(before = ?cursor.node().kind(), "goto parent before");
            cursor.goto_parent();
            tracing::debug!(after = ?cursor.node().kind(), "gotoparent" );
        }

        Some(vec![
            #[allow(deprecated)]
            DocumentSymbol {
                name: kind.name(&node, src),
                detail: None,
                kind: kind.lsp_kind().expect("lsp kind not found"),
                tags: None,
                deprecated: None,
                range: lsp_range(&node),
                selection_range: lsp_range(&node),
                children: Some(child_symbols),
            },
        ])
    })
}

pub fn get_document_symbols(cursor: &mut TreeCursor, src: &[u8]) -> Vec<DocumentSymbol> {
    let mut symbols = vec![];
    let needs_restore = if matches!(cursor.node().kind(), "source_file" | "contract_body") {
        tracing::debug!("goto_first_child");
        if cursor.goto_first_child() {
            true
        } else {
            false
        }
    } else {
        false
    };

    loop {
        tracing::debug!("node_kind: {}", cursor.node().kind());
        let symbol = get_document_symbol(cursor, src);
        if symbol.is_some() {
            symbols.extend(symbol.unwrap())
        }
        if !cursor.goto_next_sibling() {
            break;
        }
    }

    // restore cursor to original
    if needs_restore {
        cursor.goto_parent();
    }
    return symbols;
}

mod tests {
    #[test]
    fn debug_shit() {
        let src = r#"
        pragma solidity 0.8.26;
        contract Contract {
            function func() external pure returns (uint) {}
        }
        "#;
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(tree_sitter_solidity::language())
            .expect("error setting language");
        let tree = parser.parse(src, None);
        let tree = tree.expect("parsing failed");
        println!(
            "document_symbols: {:?}",
            super::get_document_symbols(&mut tree.walk(), src.as_bytes())
        );
        // assert!(false, "kind: {}", tree.walk().node().kind());
    }
}
