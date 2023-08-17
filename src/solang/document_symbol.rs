use std::default::Default;

use forge_fmt::solang_ext::pt::TypeDefinition;
use solang_parser::pt::*;
use tower_lsp::lsp_types::{DocumentSymbol, Range, SymbolKind, SymbolTag};
use tracing::instrument;

#[allow(unused_imports)]
use crate::append_to_file;
use crate::backend::Source;

pub trait ToDocumentSymbol: CodeLocation {
    fn to_document_symbol(&self, source: &Source) -> DocumentSymbol;
    fn try_to_document_symbol(&self, source: &Source) -> Option<DocumentSymbol> {
        Some(self.to_document_symbol(source))
    }
    fn with_loc(&self, source: &Source, document_symbol: DocumentSymbol) -> DocumentSymbol {
        let loc = self.loc();
        if let Ok(range) = source.loc_to_range(&loc) {
            DocumentSymbol {
                range,
                selection_range: range,
                ..document_symbol
            }
        } else {
            document_symbol
        }
    }
    fn to_document_symbol_with_loc(&self, source: &Source) -> DocumentSymbol {
        self.with_loc(source, self.to_document_symbol(source))
    }
    fn try_to_document_symbol_with_loc(&self, source: &Source) -> Option<DocumentSymbol> {
        self.try_to_document_symbol(source)
            .map(|document_symbol| self.with_loc(source, document_symbol))
    }
}

impl ToDocumentSymbol for Identifier {
    #[instrument(skip_all)]
    fn to_document_symbol(&self, source: &Source) -> DocumentSymbol {
        let mut range = Range::default();
        // trace!("to_document_symbol: {:?}", self);
        if matches!(self.loc, Loc::File(_, _, _)) {
            if let Ok(range_) = source.loc_to_range(&self.loc) {
                range = range_;
            }
        }

        DocumentSymbolBuilder::new(self.name.clone(), SymbolKind::VARIABLE)
            .range(range)
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
                    .map(|enum_value| {
                        #[allow(clippy::unwrap_used)]
                        let doc_symbol = enum_value
                            .as_ref()
                            .unwrap()
                            .to_document_symbol_with_loc(source);

                        DocumentSymbol {
                            kind: SymbolKind::ENUM_MEMBER,
                            // okay to unwrap because filtered Nones
                            ..doc_symbol
                        }
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
                        ..field.to_document_symbol_with_loc(source)
                    })
                    .collect::<Vec<DocumentSymbol>>(),
            )
            .build()
    }
}

impl ToDocumentSymbol for FunctionDefinition {
    fn to_document_symbol(&self, _source: &Source) -> DocumentSymbol {
        let ident_to_string = |name: &Option<Identifier>, default: &str| {
            name.as_ref()
                .map_or(default.to_string(), |name| name.name.clone())
        };

        let name = match self.ty {
            FunctionTy::Constructor => "constructor".to_string(),
            FunctionTy::Function => ident_to_string(&self.name, "<function>"),
            FunctionTy::Fallback => "fallback".to_string(),
            FunctionTy::Receive => "receive".to_string(),
            FunctionTy::Modifier => {
                format!("modifier {}", ident_to_string(&self.name, "<modifier>"))
            }
        };

        let mutability = self
            .attributes
            .iter()
            .filter_map(|attr| match &attr {
                FunctionAttribute::Mutability(Mutability::View(_)) => Some("view"),
                FunctionAttribute::Mutability(Mutability::Pure(_)) => Some("pure"),
                _ => None,
            })
            .collect::<Vec<&str>>()
            .first()
            .map(|x| x.to_string());

        let visibility = self
            .attributes
            .iter()
            .filter_map(|attr| match &attr {
                FunctionAttribute::Visibility(Visibility::External(_)) => Some("external"),
                FunctionAttribute::Visibility(Visibility::Public(_)) => Some("public"),
                FunctionAttribute::Visibility(Visibility::Internal(_)) => Some("internal"),
                FunctionAttribute::Visibility(Visibility::Private(_)) => Some("private"),
                _ => None,
            })
            .collect::<Vec<&str>>()
            .first()
            .map(|x| x.to_string());

        let payable = self
            .attributes
            .iter()
            .find(|attr| matches!(attr, FunctionAttribute::Mutability(Mutability::Payable(_))))
            .map(|_| "payable".to_string());

        let detail = vec![visibility, payable, mutability]
            .into_iter()
            .flatten()
            .collect::<Vec<String>>()
            .join(" ");

        DocumentSymbolBuilder::new_with_identifier(&self.name, "<function>", SymbolKind::FUNCTION)
            .name(name)
            .detail(detail)
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
                Some(struct_definition.to_document_symbol_with_loc(source))
            }
            Self::EnumDefinition(enum_definition) => {
                Some(enum_definition.to_document_symbol_with_loc(source))
            }
            Self::EventDefinition(event_definition) => {
                Some(event_definition.to_document_symbol_with_loc(source))
            }
            Self::ErrorDefinition(error_definition) => {
                Some(error_definition.to_document_symbol_with_loc(source))
            }
            Self::FunctionDefinition(func_definition) => Some(DocumentSymbol {
                kind: SymbolKind::METHOD,
                ..func_definition.to_document_symbol_with_loc(source)
            }),
            Self::VariableDefinition(variable_definition) => Some(DocumentSymbol {
                kind: SymbolKind::PROPERTY,
                ..variable_definition.to_document_symbol_with_loc(source)
            }),
            Self::TypeDefinition(type_definition) => {
                Some(type_definition.to_document_symbol_with_loc(source))
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
