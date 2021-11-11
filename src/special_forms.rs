use crate::{
    env::EnvironmentStore,
    eval,
    instruction::{Instruction, InstructionInner},
};

pub fn f_def(val: Instruction, env_id: usize, env_store: &mut EnvironmentStore) -> Instruction {
    todo!()
}

pub fn f_function(
    params: Vec<Instruction>,
    body: Instruction,
    env_id: usize,
    env_store: &mut EnvironmentStore,
) -> Instruction {
    let env_id = env_store.new_env(Some(env_id));
    let params = params
        .iter()
        .filter_map(|p| match p.inner() {
            InstructionInner::Symbol(sym) => {
                env_store.set_at(
                    env_id,
                    sym.clone(),
                    Instruction::new(InstructionInner::FromFuncall, p.span()),
                );
                Some(sym)
            }
            _ => {
                let span = p.span();
                error!(span, "Function parameter must be a symbol");
                None
            }
        })
        .collect();
    let body = eval(body, env_id, env_store);

    Instruction::new(
        InstructionInner::FunctionDefinition {
            params,
            body: body.clone(),
        },
        body.span(),
    )
}

pub fn f_macro(
    params: Vec<Instruction>,
    body: Instruction,
) -> Instruction {
    let params = params
        .iter()
        .filter_map(|p| match p.inner() {
            InstructionInner::Symbol(sym) => {
                Some(sym)
            }
            _ => {
                let span = p.span();
                error!(span, "Function parameter must be a symbol");
                None
            }
        })
        .collect();

    Instruction::new(
        InstructionInner::MacroDefinition {
            params,
            body: body.clone(),
        },
        body.span(),
    ) 
}
