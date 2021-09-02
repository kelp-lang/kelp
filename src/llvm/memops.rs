use inkwell::{AddressSpace, context::Context, module::{Linkage, Module}};

use super::Generator;

fn build_malloc(generator: &Generator) {
    let i8_type = generator.context.i8_type();
    let i8_ptr_type = i8_type.ptr_type(AddressSpace::Generic);
    let i32_type = generator.context.i64_type();
    
    let fn_type = i8_ptr_type.fn_type(&[i32_type.into()], false);
    let fn_setup = generator.module.add_function("malloc", fn_type, Some(Linkage::External));
}

fn build_free(generator: &Generator) {
    let i8_ptr_type = generator.context.i8_type().ptr_type(AddressSpace::Generic);
    let void_type = generator.context.void_type();

    let fn_type = void_type.fn_type(&[i8_ptr_type.into()], false);
    let fn_setup = generator.module.add_function("free", fn_type, Some(Linkage::External));
}

fn build_memcpy(generator: &Generator) {
    let i8_ptr_type = generator.context.i8_type().ptr_type(AddressSpace::Generic);
    let i32_type = generator.context.i32_type();

    let fn_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i8_ptr_type.into(), i32_type.into()], false);
    let fn_setup = generator.module.add_function("memcpy", fn_type, Some(Linkage::External));
}

pub(super) fn build_memops(generator: &Generator) {
    build_malloc(&generator);
    build_free(&generator);
    build_memcpy(&generator);
}