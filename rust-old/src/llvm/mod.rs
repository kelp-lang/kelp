mod primitive_io;
mod string;
mod memops;

use std::path::Path;

use inkwell::{
    attributes::AttributeLoc,
    builder::Builder,
    context::Context,
    module::{FlagBehavior, Linkage, Module},
    types::BasicTypeEnum,
    values::{ArrayValue, BasicValueEnum, FunctionValue, InstructionValue},
    AddressSpace,
};

use crate::ast::Node;

use self::{memops::build_memops, primitive_io::build_primitive_io, string::build_string};

struct Generator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> Generator<'ctx> {



    fn build_main(&self) {
        let empty_type = self.context.void_type();
        let fn_type = empty_type.fn_type(&[], false);

        let main = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(main, "entry");

        self.builder.position_at_end(entry);
        let hello = self
            .builder
            .build_global_string_ptr("Hello world", "hello")
            .as_pointer_value();

        //let puts = self.module.get_function("puts").unwrap();
        //let gets = self.module.get_function("gets").unwrap();
        //let ret = self.builder.build_call(puts, &[hello.into()], "ret");
        //let get = self.builder.build_call(gets, &[hello.into()], "ret");
        //let set = self.builder.build_call(puts, &[hello.into()], "result");

        self.builder.build_return(None);

        //let lol = i8_type.const_int(8, false);

        //let ret = self.builder.build_call(print, &[hello.into()], "res");
    }
}

    fn add_i64(generator: &Generator) {
        let i64_type = generator.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into()], false);
        let function = generator.module.add_function("add_i64", fn_type, None);
        let basic_block = generator.context.append_basic_block(function, "entry");

        generator.builder.position_at_end(basic_block);
        let (x, y) = (
            function.get_nth_param(0).unwrap().into_int_value(),
            function.get_nth_param(1).unwrap().into_int_value(),
        );

        let sum = generator.builder.build_int_add(x, y, "sum");
        generator.builder.build_return(Some(&sum));
    }

pub fn do_llvm() {
    let path = Path::new("main.bc");

    let context = Context::create();
    let std_module = context.create_module("std");

    let generator = Generator {
        context: &context,
        module: std_module,
        builder: context.create_builder(),
    };

    add_i64(&generator);
    //generator.build_main();
    build_memops(&generator);
    build_primitive_io(&generator);
    build_string(&generator);

    generator.build_main();

    generator.module.write_bitcode_to_path(&path);
}
