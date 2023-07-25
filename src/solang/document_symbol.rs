use std::default::Default;

use forge_fmt::solang_ext::pt::TypeDefinition;
use solang_parser::pt::*;
use tower_lsp::lsp_types::{DocumentSymbol, Range, SymbolKind, SymbolTag};

pub trait ToDocumentSymbol {
    fn to_document_symbol(&self) -> DocumentSymbol;
    fn try_to_document_symbol(&self) -> Option<DocumentSymbol> {
        Some(self.to_document_symbol())
    }
}

impl ToDocumentSymbol for Identifier {
    fn to_document_symbol(&self) -> DocumentSymbol {
        DocumentSymbolBuilder::new(self.name.clone(), SymbolKind::VARIABLE).build()
    }
}

impl ToDocumentSymbol for EnumDefinition {
    fn to_document_symbol(&self) -> DocumentSymbol {
        DocumentSymbolBuilder::new_with_identifier(&self.name, "<enum>", SymbolKind::ENUM)
            .children(
                self.values
                    .iter()
                    .filter(|x| x.is_some())
                    .map(|enum_value| DocumentSymbol {
                        kind: SymbolKind::ENUM_MEMBER,
                        // okay to unwrap because filtered Nones
                        ..enum_value.as_ref().unwrap().to_document_symbol()
                    })
                    .collect::<Vec<DocumentSymbol>>(),
            )
            .build()
    }
}

impl ToDocumentSymbol for VariableDefinition {
    fn to_document_symbol(&self) -> DocumentSymbol {
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
    fn to_document_symbol(&self) -> DocumentSymbol {
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
    fn to_document_symbol(&self) -> DocumentSymbol {
        DocumentSymbolBuilder::new_with_identifier(&self.name, "<struct>", SymbolKind::STRUCT)
            .children(
                self.fields
                    .iter()
                    .map(|field| DocumentSymbol {
                        kind: SymbolKind::FIELD,
                        ..field.to_document_symbol()
                    })
                    .collect::<Vec<DocumentSymbol>>(),
            )
            .build()
    }
}

impl ToDocumentSymbol for FunctionDefinition {
    fn to_document_symbol(&self) -> DocumentSymbol {
        DocumentSymbolBuilder::new_with_identifier(&self.name, "<function>", SymbolKind::FUNCTION)
            .build()
    }
}

impl ToDocumentSymbol for ErrorDefinition {
    fn to_document_symbol(&self) -> DocumentSymbol {
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
    fn to_document_symbol(&self) -> DocumentSymbol {
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
    fn to_document_symbol(&self) -> DocumentSymbol {
        DocumentSymbolBuilder::new(format!("type {}", self.name.name), SymbolKind::OBJECT).build()
    }
}

impl ToDocumentSymbol for ContractPart {
    fn to_document_symbol(&self) -> DocumentSymbol {
        unimplemented!()
    }

    fn try_to_document_symbol(&self) -> Option<DocumentSymbol> {
        match self {
            Self::StructDefinition(struct_definition) => {
                Some(struct_definition.to_document_symbol())
            }
            Self::EnumDefinition(enum_definition) => Some(enum_definition.to_document_symbol()),
            Self::EventDefinition(event_definition) => Some(event_definition.to_document_symbol()),
            Self::ErrorDefinition(error_definition) => Some(error_definition.to_document_symbol()),
            Self::FunctionDefinition(func_definition) => Some(DocumentSymbol {
                kind: SymbolKind::METHOD,
                ..func_definition.to_document_symbol()
            }),
            Self::VariableDefinition(variable_definition) => Some(DocumentSymbol {
                kind: SymbolKind::PROPERTY,
                ..variable_definition.to_document_symbol()
            }),
            Self::TypeDefinition(type_definition) => Some(type_definition.to_document_symbol()),
            _ => None,
        }
    }
}

impl ToDocumentSymbol for ContractDefinition {
    fn to_document_symbol(&self) -> DocumentSymbol {
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
                    .map(|part| part.try_to_document_symbol())
                    .filter(|x| x.is_some())
                    // okay to unwrap because filtered Nones
                    .map(|x| x.unwrap())
                    .collect(),
            )
            .build()
    }
}

struct DocumentSymbolBuilder(DocumentSymbol);

impl DocumentSymbolBuilder {
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

    fn name(&mut self, name: String) -> &mut Self {
        self.0.name = name;
        self
    }

    fn kind(&mut self, kind: SymbolKind) -> &mut Self {
        self.0.kind = kind;
        self
    }

    fn detail(&mut self, detail: String) -> &mut Self {
        self.0.detail = Some(detail);
        self
    }

    fn tags(&mut self, tags: Vec<SymbolTag>) -> &mut Self {
        self.0.tags = Some(tags);
        self
    }

    fn deprecated(&mut self, deprecated: bool) -> &mut Self {
        self.0.deprecated = Some(deprecated);
        self
    }

    fn range(&mut self, range: Range) -> &mut Self {
        self.0.range = range;
        self
    }

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
