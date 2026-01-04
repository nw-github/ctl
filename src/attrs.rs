use crate::{
    Error, Located,
    ast::{AttrName as AN, Attribute, Attributes},
    intern::{StrId, Strings},
    project::Project,
};

#[derive(Default)]
pub struct VariableAttrs {
    pub link_name: Option<StrId>,
    pub thread_local: bool,
}

fn one_str_arg(attr: &Attribute, name: StrId, proj: &mut Project) -> Option<StrId> {
    let Some(prop) = attr.props.first() else {
        proj.diag.report(Error::new(
            format!("attribute '{}' requires an argument", proj.strings.resolve(&name)),
            attr.name.span,
        ));
        return None;
    };

    let AN::Str(id) = prop.name.data else {
        proj.diag.report(Error::new(
            format!("attribute '{}' requires a string argument", proj.strings.resolve(&name)),
            prop.name.span,
        ));
        return None;
    };

    Some(id)
}

fn opt_str_arg(attr: &Attribute, name: StrId, proj: &mut Project) -> Option<Located<StrId>> {
    let prop = attr.props.first()?;
    let AN::Str(id) = prop.name.data else {
        proj.diag.report(Error::new(
            format!("attribute '{}' requires a string argument", proj.strings.resolve(&name)),
            prop.name.span,
        ));
        return None;
    };

    Some(Located::new(prop.name.span, id))
}

fn unrecognized(attr: &Attribute, proj: &mut Project) {
    if matches!(attr.name.data, AN::Str(Strings::ATTR_FEATURE)) {
        return;
    }

    proj.diag
        .report(Error::invalid_attr(attr.name.data.as_str_data(&proj.strings), attr.name.span));
}

fn check_intrinsic(name: StrId, attr: &Attribute, proj: &mut Project) -> Option<StrId> {
    let (name, span) = if let Some(attr) = attr.props.first()
        && let Some(id) = attr.name.data.as_str()
    {
        (*id, attr.name.span)
    } else {
        (name, attr.name.span)
    };

    let data = proj.strings.resolve(&name);
    #[rustfmt::skip]
    if !matches!(
        data,
        "size_of" | "align_of" | "panic" | "binary_op" | "unary_op" | "numeric_cast" | "numeric_abs"
            | "max_value" | "min_value" | "unreachable_unchecked" | "type_id" | "type_name"
            | "read_volatile" | "write_volatile" | "source_location" | "ptr_add_signed"
            | "ptr_add_unsigned" | "ptr_sub_signed" | "ptr_sub_unsigned" | "ptr_diff"
            | "builtin_dbg"
    ) {
        proj.diag.report(Error::new(format!("intrinsic '{data}' is not supported"), span));
        return None;
    };

    Some(name)
}

impl VariableAttrs {
    pub fn relevant(attrs: &Attributes, proj: &mut Project) -> Self {
        let mut this = Self::default();
        for attr in attrs.iter() {
            match attr.name.data {
                AN::Str(id @ Strings::ATTR_LINKNAME) => {
                    this.link_name = one_str_arg(attr, id, proj)
                }
                AN::Str(Strings::ATTR_THREAD_LOCAL) => this.thread_local = true,
                _ => unrecognized(attr, proj),
            }
        }
        this
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FunctionInline {
    Always,
    Never,
    Encouraged,
}

#[derive(Default)]
pub struct FunctionAttrs {
    pub panic_handler: bool,
    pub test_runner: bool,
    pub cold: bool,
    pub safe_extern: bool,
    pub link_name: Option<StrId>,
    pub test_skip: Option<Option<StrId>>,
    pub inline: Option<FunctionInline>,
    pub intrinsic: Option<StrId>,
    pub macro_name: Option<StrId>,
}

impl FunctionAttrs {
    pub fn relevant(fn_name: StrId, attrs: &Attributes, proj: &mut Project) -> Self {
        let mut this = Self::default();
        for attr in attrs.iter() {
            let Some(&id) = attr.name.data.as_str() else {
                unrecognized(attr, proj);
                continue;
            };

            match id {
                Strings::ATTR_TEST_RUNNER => this.test_runner = true,
                Strings::ATTR_PANIC_HANDLER => this.panic_handler = true,
                Strings::ATTR_MACRO => {
                    if attr.props.is_empty() {
                        this.macro_name = Some(fn_name);
                    } else {
                        this.macro_name = one_str_arg(attr, id, proj);
                    }
                }
                Strings::ATTR_COLD => this.cold = true,
                Strings::ATTR_LINKNAME => this.link_name = one_str_arg(attr, id, proj),
                Strings::SKIP => this.test_skip = Some(opt_str_arg(attr, id, proj).map(|s| s.data)),
                Strings::ATTR_SAFE => this.safe_extern = true,
                Strings::ATTR_INLINE => {
                    if let Some(arg) = opt_str_arg(attr, id, proj) {
                        this.inline = Some(match proj.strings.resolve(&arg.data) {
                            "always" => FunctionInline::Always,
                            "never" => FunctionInline::Never,
                            _ => {
                                proj.diag.report(Error::new(
                                    "expected 'always', 'never', or nothing",
                                    arg.span,
                                ));
                                FunctionInline::Encouraged
                            }
                        });
                    } else {
                        this.inline = Some(FunctionInline::Encouraged);
                    }
                }
                Strings::ATTR_INTRINSIC => this.intrinsic = check_intrinsic(fn_name, attr, proj),
                _ => unrecognized(attr, proj),
            }
        }
        this
    }
}

#[derive(Default)]
pub struct UserTypeAttrs {
    pub lang: Option<StrId>,
    pub align: Option<usize>,
}

impl UserTypeAttrs {
    pub fn relevant(attrs: &Attributes, proj: &mut Project) -> Self {
        let mut this = Self::default();
        for attr in attrs.iter() {
            match attr.name.data {
                AN::Str(id @ Strings::ATTR_LANG) => this.lang = one_str_arg(attr, id, proj),
                AN::Str(id @ Strings::ATTR_ALIGN) => {
                    let mut span = attr.name.span;
                    let Some(int) = attr.props.first().and_then(|prop| {
                        span = prop.name.span;
                        prop.name.data.as_int()
                    }) else {
                        proj.diag.report(Error::new(
                            format!(
                                "attribute '{}' requires an integer argument",
                                proj.strings.resolve(&id)
                            ),
                            attr.name.span,
                        ));
                        continue;
                    };

                    let Some(ival) = int.clone().into_word::<usize>() else {
                        proj.diag.report(Error::new(
                            format!("alignment '{int}' is too large"),
                            attr.name.span,
                        ));
                        continue;
                    };

                    if ival == 0 || !ival.is_power_of_two() {
                        proj.diag.report(Error::new(
                            format!("alignment '{int}' is not a non-zero power of two"),
                            attr.name.span,
                        ));
                        continue;
                    }

                    this.align = Some(ival);
                }
                _ => unrecognized(attr, proj),
            }
        }
        this
    }
}
