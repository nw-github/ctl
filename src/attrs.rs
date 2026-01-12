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

fn one_str_arg_l(attr: &Attribute, name: StrId, proj: &mut Project) -> Option<Located<StrId>> {
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

    Some(Located::new(prop.name.span, id))
}

fn one_str_arg(attr: &Attribute, name: StrId, proj: &mut Project) -> Option<StrId> {
    one_str_arg_l(attr, name, proj).map(|l| l.data)
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

fn check_intrinsic(name: StrId, attr: &Attribute, proj: &mut Project) -> Option<Intrinsic> {
    let (name, span) = if let Some(attr) = attr.props.first()
        && let Some(id) = attr.name.data.as_str()
    {
        (*id, attr.name.span)
    } else {
        (name, attr.name.span)
    };

    let data = proj.strings.resolve(&name);
    if let Some(intrinsic) = Intrinsic::from_str(data) {
        return Some(intrinsic);
    }

    proj.diag.report(Error::new(format!("intrinsic '{data}' is not supported"), span));
    None
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
    pub export: bool,
    pub link_name: Option<StrId>,
    pub test_skip: Option<Option<StrId>>,
    pub inline: Option<FunctionInline>,
    pub intrinsic: Option<Intrinsic>,
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
                Strings::ATTR_SKIP => {
                    this.test_skip = Some(opt_str_arg(attr, id, proj).map(|s| s.data))
                }
                Strings::ATTR_SAFE => this.safe_extern = true,
                Strings::ATTR_EXPORT => this.export = true,
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

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum Layout {
    #[default]
    Auto,
    C,
}

#[derive(Default)]
pub struct UserTypeAttrs {
    pub lang: Option<LangType>,
    pub align: Option<usize>,
    pub layout: Layout,
}

impl UserTypeAttrs {
    pub fn relevant(attrs: &Attributes, proj: &mut Project) -> Self {
        let mut this = Self::default();
        for attr in attrs.iter() {
            match attr.name.data {
                AN::Str(id @ Strings::ATTR_LANG) => {
                    let Some(arg) = one_str_arg_l(attr, id, proj) else {
                        continue;
                    };

                    let Some(ty) = LangType::from_str(proj.str(arg.data)) else {
                        proj.diag.report(Error::new(
                            format!("unknown lang type '{}'", proj.str(arg.data)),
                            arg.span,
                        ));
                        continue;
                    };

                    this.lang = Some(ty);
                }
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
                AN::Str(id @ Strings::ATTR_LAYOUT) => {
                    let Some(arg) = one_str_arg_l(attr, id, proj) else {
                        continue;
                    };

                    this.layout = match proj.strings.resolve(&arg.data) {
                        "C" => Layout::C,
                        "auto" => Layout::Auto,
                        other => {
                            proj.diag.report(Error::new(
                                format!("invalid layout '{other}', expected 'C' or 'auto'"),
                                arg.span,
                            ));
                            continue;
                        }
                    };
                }
                _ => unrecognized(attr, proj),
            }
        }
        this
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangType {
    Span,
    SpanMut,
    Vec,
    Set,
    Map,
    String,
    Option,
    FmtArg,
    FmtArgs,
    TestInfo,
    Mutable,
    Ordering,

    Range,
    RangeFull,
    RangeTo,
    RangeToInclusive,
    RangeFrom,
    RangeInclusive,

    Numeric,
    Integral,
    Signed,
    Unsigned,
    Array,
    Debug,
    Format,
    Pointer,
    Write,
    Iterator,
    Tuple,
    FnPtr,

    OpCmp,
    OpEq,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpRem,
    OpAnd,
    OpOr,
    OpXor,
    OpShl,
    OpShr,
    OpAddAssign,
    OpSubAssign,
    OpMulAssign,
    OpDivAssign,
    OpRemAssign,
    OpAndAssign,
    OpOrAssign,
    OpXorAssign,
    OpShlAssign,
    OpShrAssign,
    OpNeg,
    OpNot,
    OpUnwrap,
    OpDec,
    OpInc,
    OpFn,

    FallbackDebug,
}

impl LangType {
    fn from_str(s: &str) -> Option<LangType> {
        match s {
            "span" => Some(Self::Span),
            "span_mut" => Some(Self::SpanMut),
            "vec" => Some(Self::Vec),
            "set" => Some(Self::Set),
            "map" => Some(Self::Map),
            "string" => Some(Self::String),
            "option" => Some(Self::Option),
            "fmt_arg" => Some(Self::FmtArg),
            "fmt_args" => Some(Self::FmtArgs),
            "test_info" => Some(Self::TestInfo),
            "mutable" => Some(Self::Mutable),
            "ordering" => Some(Self::Ordering),
            "range" => Some(Self::Range),
            "range_full" => Some(Self::RangeFull),
            "range_to" => Some(Self::RangeTo),
            "range_to_inclusive" => Some(Self::RangeToInclusive),
            "range_from" => Some(Self::RangeFrom),
            "range_inclusive" => Some(Self::RangeInclusive),
            "numeric" => Some(Self::Numeric),
            "integral" => Some(Self::Integral),
            "signed" => Some(Self::Signed),
            "unsigned" => Some(Self::Unsigned),
            "array" => Some(Self::Array),
            "tuple" => Some(Self::Tuple),
            "fn_ptr" => Some(Self::FnPtr),
            "fmt_debug" => Some(Self::Debug),
            "fmt_format" => Some(Self::Format),
            "fmt_pointer" => Some(Self::Pointer),
            "fmt_write" => Some(Self::Write),
            "iter" => Some(Self::Iterator),
            "op_cmp" => Some(Self::OpCmp),
            "op_eq" => Some(Self::OpEq),
            "op_add" => Some(Self::OpAdd),
            "op_sub" => Some(Self::OpSub),
            "op_mul" => Some(Self::OpMul),
            "op_div" => Some(Self::OpDiv),
            "op_rem" => Some(Self::OpRem),
            "op_and" => Some(Self::OpAnd),
            "op_or" => Some(Self::OpOr),
            "op_xor" => Some(Self::OpXor),
            "op_shl" => Some(Self::OpShl),
            "op_shr" => Some(Self::OpShr),
            "op_add_assign" => Some(Self::OpAddAssign),
            "op_sub_assign" => Some(Self::OpSubAssign),
            "op_mul_assign" => Some(Self::OpMulAssign),
            "op_div_assign" => Some(Self::OpDivAssign),
            "op_rem_assign" => Some(Self::OpRemAssign),
            "op_and_assign" => Some(Self::OpAndAssign),
            "op_or_assign" => Some(Self::OpOrAssign),
            "op_xor_assign" => Some(Self::OpXorAssign),
            "op_shl_assign" => Some(Self::OpShlAssign),
            "op_shr_assign" => Some(Self::OpShrAssign),
            "op_neg" => Some(Self::OpNeg),
            "op_not" => Some(Self::OpNot),
            "op_unwrap" => Some(Self::OpUnwrap),
            "op_dec" => Some(Self::OpDec),
            "op_inc" => Some(Self::OpInc),
            "op_fn" => Some(Self::OpFn),
            "fallback_debug" => Some(Self::FallbackDebug),
            _ => None,
        }
    }
}

impl std::fmt::Display for LangType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Intrinsic {
    NumericCast,
    MaxValue,
    MinValue,
    SizeOf,
    AlignOf,
    Panic,
    UnreachableUnchecked,
    BinaryOp,
    UnaryOp,
    TypeId,
    TypeName,
    ReadVolatile,
    WriteVolatile,
    SourceLocation,
    PtrAddSigned,
    PtrAddUnsigned,
    PtrSubSigned,
    PtrSubUnsigned,
    PtrDiff,
    BuiltinDbg,
    InvokeWithTuple,
}

impl Intrinsic {
    fn from_str(s: &str) -> Option<Intrinsic> {
        match s {
            "numeric_cast" => Some(Self::NumericCast),
            "max_value" => Some(Self::MaxValue),
            "min_value" => Some(Self::MinValue),
            "size_of" => Some(Self::SizeOf),
            "align_of" => Some(Self::AlignOf),
            "panic" => Some(Self::Panic),
            "unreachable_unchecked" => Some(Self::UnreachableUnchecked),
            "binary_op" => Some(Self::BinaryOp),
            "unary_op" => Some(Self::UnaryOp),
            "type_id" => Some(Self::TypeId),
            "type_name" => Some(Self::TypeName),
            "read_volatile" => Some(Self::ReadVolatile),
            "write_volatile" => Some(Self::WriteVolatile),
            "source_location" => Some(Self::SourceLocation),
            "ptr_add_signed" => Some(Self::PtrAddSigned),
            "ptr_add_unsigned" => Some(Self::PtrAddUnsigned),
            "ptr_sub_signed" => Some(Self::PtrSubSigned),
            "ptr_sub_unsigned" => Some(Self::PtrSubUnsigned),
            "ptr_diff" => Some(Self::PtrDiff),
            "builtin_dbg" => Some(Self::BuiltinDbg),
            "invoke_with_tuple" => Some(Self::InvokeWithTuple),
            _ => None,
        }
    }
}

impl std::fmt::Display for Intrinsic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
