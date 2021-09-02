use inkwell::{AddressSpace, values::BasicValueEnum};

use super::Generator;

fn build_string_type(generator: &Generator) {
    let i8_ptr_type = generator.context.i8_type().ptr_type(AddressSpace::Generic);
    let i64_type = generator.context.i64_type();
    let opaque_struct_type = generator.context.opaque_struct_type("String");
    opaque_struct_type.set_body(&[i8_ptr_type.into(), i64_type.into(), i64_type.into(), i64_type.into()], false);

    //println!("{:?} {:?}", struct_type, generator.module.get_struct_type("String"));
    //let struct_setup = generator.module.add_global(struct_type, Some(AddressSpace::Generic), "String");
}

fn build_string_create_default(generator: &Generator) {
    let string_type = generator.module.get_struct_type("String").unwrap();
    let string_ptr_type = string_type.ptr_type(AddressSpace::Generic);
    let void_type = generator.context.void_type();
    let i64_type = generator.context.i64_type();
    let i8_ptr_type = generator.context.i8_type().ptr_type(AddressSpace::Generic);

    let fn_type = void_type.fn_type(&[string_ptr_type.into()], false);
    let fn_setup = generator.module.add_function("String_Create_Default", fn_type, None);
    let basic_block = generator.context.append_basic_block(fn_setup, "entry");

    generator.builder.position_at_end(basic_block);
    let this = fn_setup.get_first_param().unwrap().into_pointer_value();
    let buffer = generator.builder.build_struct_gep(this, 0, "buffer").unwrap();
    let length = generator.builder.build_struct_gep(this, 1, "length").unwrap();
    let maxlen = generator.builder.build_struct_gep(this, 2, "maxlen").unwrap();
    let factor = generator.builder.build_struct_gep(this, 3, "factor").unwrap();

    let null = i8_ptr_type.const_null();
    let zero = i64_type.const_int(0, false);
    let sixteen = i64_type.const_int(16, false);

    generator.builder.build_store(buffer, null);
    generator.builder.build_store(length, zero);
    generator.builder.build_store(maxlen, zero);
    generator.builder.build_store(factor, sixteen);

    generator.builder.build_return(None);
}

pub(super) fn build_string (generator: &Generator) {
    build_string_type(&generator);
    build_string_create_default(&generator);
}