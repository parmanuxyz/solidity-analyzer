use std::default::Default;

use forge_fmt::solang_ext::pt::TypeDefinition;
use solang_parser::pt::*;
use tower_lsp::lsp_types::{DocumentSymbol, Range, SymbolKind, SymbolTag};

use crate::append_to_file;
use crate::backend::Source;

pub trait ToDocumentSymbol {
    fn to_document_symbol(&self, source: &Source) -> DocumentSymbol;
    fn try_to_document_symbol(&self, source: &Source) -> Option<DocumentSymbol> {
        Some(self.to_document_symbol(source))
    }
}

impl ToDocumentSymbol for Identifier {
    fn to_document_symbol(&self, source: &Source) -> DocumentSymbol {
        let mut range = Range::default();
        append_to_file!("/Users/meet/solidity-analyzer.log", "loc: {:?}", self.loc);
        if matches!(self.loc, Loc::File(_, _, _)) {
            if let Ok(range_) = source.loc_to_range(&self.loc) {
                range = range_;
            }
        }

        DocumentSymbolBuilder::new(self.name.clone(), SymbolKind::VARIABLE)
            .range(range.clone())
            .selection_range(range)
            .build()
    }
}

impl ToDocumentSymbol for EnumDefinition {
    fn to_document_symbol(&self, source: &Source) -> DocumentSymbol {
        DocumentSymbolBuilder::new_with_identifier(&self.name, "<enum>", SymbolKind::ENUM)
            .children(
                self.values
                    .iter()
                    .filter(|x| x.is_some())
                    .map(|enum_value| DocumentSymbol {
                        kind: SymbolKind::ENUM_MEMBER,
                        // okay to unwrap because filtered Nones
                        ..enum_value.as_ref().unwrap().to_document_symbol(source)
                    })
                    .collect::<Vec<DocumentSymbol>>(),
            )
            .build()
    }
}

impl ToDocumentSymbol for VariableDefinition {
    fn to_document_symbol(&self, _source: &Source) -> DocumentSymbol {
        let mut builder =
            DocumentSymbolBuilder::new_with_identifier(&self.name, "<var>", SymbolKind::VARIABLE);

        if matches!(self.ty, Expression::Type(_, _)) {
            builder.detail(self.ty.to_string()).build()
        } else {
            builder.build()
        }
    }
}

impl ToDocumentSymbol for VariableDeclaration {
    fn to_document_symbol(&self, _source: &Source) -> DocumentSymbol {
        let mut builder =
            DocumentSymbolBuilder::new_with_identifier(&self.name, "<var>", SymbolKind::VARIABLE);

        if matches!(self.ty, Expression::Type(_, _)) {
            builder.detail(self.ty.to_string()).build()
        } else {
            builder.build()
        }
    }
}

impl ToDocumentSymbol for StructDefinition {
    fn to_document_symbol(&self, source: &Source) -> DocumentSymbol {
        DocumentSymbolBuilder::new_with_identifier(&self.name, "<struct>", SymbolKind::STRUCT)
            .children(
                self.fields
                    .iter()
                    .map(|field| DocumentSymbol {
                        kind: SymbolKind::FIELD,
                        ..field.to_document_symbol(source)
                    })
                    .collect::<Vec<DocumentSymbol>>(),
            )
            .build()
    }
}

impl ToDocumentSymbol for FunctionDefinition {
    fn to_document_symbol(&self, _source: &Source) -> DocumentSymbol {
        DocumentSymbolBuilder::new_with_identifier(&self.name, "<function>", SymbolKind::FUNCTION)
            .build()
    }
}

impl ToDocumentSymbol for ErrorDefinition {
    fn to_document_symbol(&self, _source: &Source) -> DocumentSymbol {
        DocumentSymbolBuilder::new(
            format!(
                "error {}",
                self.name
                    .as_ref()
                    .map_or("<error>".to_string(), |ident| ident.name.to_string())
            ),
            SymbolKind::OBJECT,
        )
        .build()
    }
}

impl ToDocumentSymbol for EventDefinition {
    fn to_document_symbol(&self, _source: &Source) -> DocumentSymbol {
        DocumentSymbolBuilder::new(
            format!(
                "event {}",
                self.name
                    .as_ref()
                    .map_or("<event>".to_string(), |ident| ident.name.to_string())
            ),
            SymbolKind::EVENT,
        )
        .build()
    }
}

impl ToDocumentSymbol for TypeDefinition {
    fn to_document_symbol(&self, _source: &Source) -> DocumentSymbol {
        DocumentSymbolBuilder::new(format!("type {}", self.name.name), SymbolKind::OBJECT).build()
    }
}

impl ToDocumentSymbol for ContractPart {
    fn to_document_symbol(&self, _source: &Source) -> DocumentSymbol {
        unimplemented!()
    }

    fn try_to_document_symbol(&self, source: &Source) -> Option<DocumentSymbol> {
        match self {
            Self::StructDefinition(struct_definition) => {
                Some(struct_definition.to_document_symbol(source))
            }
            Self::EnumDefinition(enum_definition) => {
                Some(enum_definition.to_document_symbol(source))
            }
            Self::EventDefinition(event_definition) => {
                Some(event_definition.to_document_symbol(source))
            }
            Self::ErrorDefinition(error_definition) => {
                Some(error_definition.to_document_symbol(source))
            }
            Self::FunctionDefinition(func_definition) => Some(DocumentSymbol {
                kind: SymbolKind::METHOD,
                ..func_definition.to_document_symbol(source)
            }),
            Self::VariableDefinition(variable_definition) => Some(DocumentSymbol {
                kind: SymbolKind::PROPERTY,
                ..variable_definition.to_document_symbol(source)
            }),
            Self::TypeDefinition(type_definition) => {
                Some(type_definition.to_document_symbol(source))
            }
            _ => None,
        }
    }
}

impl ToDocumentSymbol for ContractDefinition {
    fn to_document_symbol(&self, source: &Source) -> DocumentSymbol {
        let kind = match self.ty {
            ContractTy::Abstract(_) | ContractTy::Contract(_) | ContractTy::Library(_) => {
                SymbolKind::CLASS
            }
            ContractTy::Interface(_) => SymbolKind::INTERFACE,
        };

        DocumentSymbolBuilder::new_with_identifier(&self.name, "<contract>", kind)
            .children(
                self.parts
                    .iter()
                    .filter_map(|part| part.try_to_document_symbol(source))
                    .collect(),
            )
            .build()
    }
}

struct DocumentSymbolBuilder(DocumentSymbol);

impl DocumentSymbolBuilder {
    #[allow(deprecated)]
    fn new_with_identifier(
        name: &Option<Identifier>,
        default_name: &str,
        kind: SymbolKind,
    ) -> Self {
        Self(DocumentSymbol {
            name: name
                .as_ref()
                .map_or(default_name.to_string(), |name| name.name.clone()),
            kind,
            detail: Default::default(),
            tags: Default::default(),
            deprecated: Default::default(),
            range: Default::default(),
            selection_range: Default::default(),
            children: Default::default(),
        })
    }

    #[allow(deprecated)]
    fn new(name: String, kind: SymbolKind) -> Self {
        Self(DocumentSymbol {
            name,
            kind,
            detail: Default::default(),
            tags: Default::default(),
            deprecated: Default::default(),
            range: Default::default(),
            selection_range: Default::default(),
            children: Default::default(),
        })
    }

    #[allow(dead_code)]
    fn name(&mut self, name: String) -> &mut Self {
        self.0.name = name;
        self
    }

    #[allow(dead_code)]
    fn kind(&mut self, kind: SymbolKind) -> &mut Self {
        self.0.kind = kind;
        self
    }

    fn detail(&mut self, detail: String) -> &mut Self {
        self.0.detail = Some(detail);
        self
    }

    #[allow(dead_code)]
    fn tags(&mut self, tags: Vec<SymbolTag>) -> &mut Self {
        self.0.tags = Some(tags);
        self
    }

    #[allow(dead_code)]
    #[allow(deprecated)]
    fn deprecated(&mut self, deprecated: bool) -> &mut Self {
        self.0.deprecated = Some(deprecated);
        self
    }

    #[allow(dead_code)]
    fn range(&mut self, range: Range) -> &mut Self {
        self.0.range = range;
        self
    }

    #[allow(dead_code)]
    fn selection_range(&mut self, selection_range: Range) -> &mut Self {
        self.0.selection_range = selection_range;
        self
    }

    fn children(&mut self, children: Vec<DocumentSymbol>) -> &mut Self {
        self.0.children = Some(children);
        self
    }

    fn build(&self) -> DocumentSymbol {
        self.0.clone()
    }
}
