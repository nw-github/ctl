use crate::{
    Error,
    ast::{Attribute, Attributes},
    intern::{StrId, Strings},
    project::Project,
};

#[derive(Default)]
pub struct VariableAttrs {
    pub link_name: Option<StrId>,
    pub thread_local: bool,
}

fn one_arg(attr: &Attribute, proj: &mut Project) -> Option<StrId> {
    let Some(name) = attr.props.first() else {
        proj.diag.report(Error::new(
            format!("attribute '{}' requires an argument", proj.strings.resolve(&attr.name.data)),
            attr.name.span,
        ));
        return None;
    };

    Some(name.name.data)
}

fn unrecognized(attr: &Attribute, proj: &mut Project) {
    if matches!(attr.name.data, Strings::ATTR_FEATURE) {
        return;
    }

    proj.diag.report(Error::invalid_attr(proj.strings.resolve(&attr.name.data), attr.name.span));
}

fn check_intrinsic(name: StrId, attr: &Attribute, proj: &mut Project) -> Option<StrId> {
    let (name, span) = if let Some(attr) = attr.props.first() {
        (attr.name.data, attr.name.span)
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
                Strings::ATTR_LINKNAME => this.link_name = one_arg(attr, proj),
                Strings::ATTR_THREAD_LOCAL => this.thread_local = true,
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
    pub no_gen: bool,
    pub cold: bool,
    pub safe_extern: bool,
    pub link_name: Option<StrId>,
    pub test_skip: Option<Option<StrId>>,
    pub inline: Option<FunctionInline>,
    pub intrinsic: Option<StrId>,
}

impl FunctionAttrs {
    pub fn relevant(fn_name: StrId, attrs: &Attributes, proj: &mut Project) -> Self {
        let mut this = Self::default();
        for attr in attrs.iter() {
            match attr.name.data {
                Strings::ATTR_TEST_RUNNER => this.test_runner = true,
                Strings::ATTR_PANIC_HANDLER => this.panic_handler = true,
                Strings::ATTR_NOGEN => this.no_gen = true,
                Strings::ATTR_COLD => this.cold = true,
                Strings::ATTR_LINKNAME => this.link_name = one_arg(attr, proj),
                Strings::SKIP => {
                    this.test_skip = Some(attr.props.first().map(|attr| attr.name.data));
                }
                Strings::ATTR_SAFE => this.safe_extern = true,
                Strings::ATTR_INLINE => {
                    this.inline = Some(
                        attr.props
                            .first()
                            .map(|attr| match proj.strings.resolve(&attr.name.data) {
                                "always" => FunctionInline::Always,
                                "never" => FunctionInline::Never,
                                _ => {
                                    proj.diag.report(Error::new(
                                        "expected 'always', 'never', or nothing",
                                        attr.name.span,
                                    ));
                                    FunctionInline::Encouraged
                                }
                            })
                            .unwrap_or(FunctionInline::Encouraged),
                    );
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
    pub no_gen: bool,
    pub link_name: Option<StrId>,
    pub lang: Option<StrId>,
}

impl UserTypeAttrs {
    pub fn relevant(attrs: &Attributes, proj: &mut Project) -> Self {
        let mut this = Self::default();
        for attr in attrs.iter() {
            match attr.name.data {
                Strings::ATTR_LINKNAME => this.link_name = one_arg(attr, proj),
                Strings::ATTR_NOGEN => this.no_gen = true,
                Strings::ATTR_LANG => this.lang = one_arg(attr, proj),
                _ => unrecognized(attr, proj),
            }
        }
        this
    }
}
