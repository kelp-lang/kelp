use inkwell::{
    context::Context,
    module::{Linkage, Module},
    AddressSpace,
};

use super::Generator;

fn build_puts<'ctx>(generator: &Generator<'ctx>) {
    let i64_type = generator.context.i64_type();
    let i8_ptr_type = generator.context.i8_type().ptr_type(AddressSpace::Generic);
    let fn_type = i64_type.fn_type(&[i8_ptr_type.into()], false);

    let func_setup = generator
        .module
        .add_function("puts", fn_type, Some(Linkage::External));
}

fn build_gets<'ctx>(generator: &Generator<'ctx>) {
    let i8_type = generator.context.i8_type();
    let i8_ptr_type = i8_type.ptr_type(AddressSpace::Generic);

    let fn_type = i8_type.fn_type(&[i8_ptr_type.into()], false);

    let func_setup = generator
        .module
        .add_function("gets", fn_type, Some(Linkage::External));
}

pub(super) fn build_primitive_io(generator: &Generator) {
    build_gets(&generator);
    build_puts(&generator);
}
