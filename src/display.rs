use std::fmt::{Display, Write};

use crate::{ClassType, TypeParameter};

use super::{
    BaseType, ClassSignature, FieldSignature, JavaType, MethodSignature, ReferenceType,
    SimpleClassType, ThrowsType, TypeArgument,
};

impl<'a> Display for FieldSignature<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        render_reference_type(&self.0, f)
    }
}

impl<'a> Display for ClassSignature<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        render_type_parameters(&self.type_params, f)?;
        render_class_type(&self.super_class, f)?;
        for ty in &self.super_ifaces {
            render_class_type(ty, f)?;
        }
        Ok(())
    }
}

impl<'a> Display for MethodSignature<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        render_type_parameters(&self.type_params, f)?;
        f.write_char('(')?;
        for ty in &self.parameters {
            render_java_type(ty, f)?;
        }
        f.write_char(')')?;
        match self.result {
            crate::ResultType::VoidType => f.write_char('V'),
            crate::ResultType::ValueType(ref ty) => render_java_type(ty, f),
        }?;
        for ty in &self.throws {
            f.write_char('^')?;
            match ty {
                ThrowsType::ClassType(ref ty) => render_class_type(ty, f),
                ThrowsType::TypeVariable(name) => render_type_variable(name, f),
            }?;
        }
        Ok(())
    }
}

fn render_type_variable(name: &str, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "T{};", name)
}

fn render_reference_type(
    ty: &ReferenceType<'_>,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    match ty {
        ReferenceType::Class(ty) => render_class_type(ty, f),
        ReferenceType::Variable(name) => render_type_variable(name, f),
        ReferenceType::Array(arr) => {
            for _ in 0..arr.dimension {
                f.write_char('[')?;
            }
            render_java_type(&arr.ty, f)
        }
    }
}

fn render_class_type(ty: &ClassType<'_>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_char('L')?;
    render_simple_class_type(&ty.base, f)?;
    for ty in &ty.nested {
        f.write_char('.')?;
        render_simple_class_type(ty, f)?;
    }
    f.write_char(';')
}

fn render_type_parameters(
    tys: &[TypeParameter<'_>],
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    if !tys.is_empty() {
        f.write_char('<')?;
        for ty in tys {
            f.write_str(ty.name)?;
            f.write_char(':')?;
            if let Some(ref ty) = ty.class_bound {
                render_reference_type(ty, f)?;
            }
            for ty in &ty.iface_bounds {
                f.write_char(':')?;
                render_reference_type(ty, f)?;
            }
        }
        f.write_char('>')?;
    }
    Ok(())
}

fn render_java_type(ty: &JavaType<'_>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match ty {
        JavaType::Base(ty) => f.write_char(match ty {
            BaseType::Byte => 'B',
            BaseType::Char => 'C',
            BaseType::Double => 'D',
            BaseType::Float => 'F',
            BaseType::Int => 'I',
            BaseType::Long => 'J',
            BaseType::Short => 'S',
            BaseType::Boolean => 'Z',
        }),
        JavaType::Reference(ty) => render_reference_type(ty, f),
    }
}

fn render_simple_class_type(
    ty: &SimpleClassType,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    f.write_str(ty.name)?;
    if !ty.type_args.is_empty() {
        f.write_char('<')?;
        for ty in &ty.type_args {
            match ty {
                TypeArgument::Unbounded => {
                    f.write_char('*')?;
                }
                TypeArgument::Default(ty) => {
                    render_reference_type(ty, f)?;
                }
                TypeArgument::Extends(ty) => {
                    f.write_char('+')?;
                    render_reference_type(ty, f)?;
                }
                TypeArgument::Super(ty) => {
                    f.write_char('-')?;
                    render_reference_type(ty, f)?;
                }
            }
        }
        f.write_char('>')?;
    }
    Ok(())
}
