use crate::asm::{AsmBuilder, Imm, Mem, MemSize, Reg};

pub fn intrisic(name: &str, code: &mut AsmBuilder) {
    match name {
        "io" => {
            putchar(code);
            print_int(code);
        }
        _ => {}
    }
}

fn putchar(code: &mut AsmBuilder) {
    code.label("putchar");
    code.mov(Reg::Eax, Imm::Int32(1)); // sys_write call
    code.mov(Reg::Edi, Imm::Int32(1)); // file descriptor
    code.lea(Reg::Rsi, Mem::offset(Reg::Rsp, 8, MemSize::DWord)); // *buffer
    code.mov(Reg::Edx, Imm::Int32(1)); // count
    code.syscall();
    code.ret(4);
}

fn print_int(code: &mut AsmBuilder) {
    code.label("printInt");
    code.push_sf();

    let param = Mem::offset(Reg::Rbp, 16, MemSize::DWord);

    code.mov(Reg::Rcx, Imm::Int64(0)); // counter

    code.mov(Reg::Eax, param); // quotient
    code.mov(Reg::Ebx, Imm::Int32(10)); // divisor
    code.mov(Reg::Edx, Imm::Int32(0)); // remainder

    code.label(".loop");

    code.xor(Reg::Edx, Reg::Edx);
    code.idiv(Reg::Ebx);

    code.inc(Reg::Rcx);

    code.sub(Reg::Rsp, Imm::Int64(1));
    code.mov(Mem::reg(Reg::Rsp, MemSize::Byte), Reg::Dl);
    code.add(Mem::reg(Reg::Rsp, MemSize::Byte), Imm::Char('0'));

    code.cmp(Reg::Eax, Imm::Int32(0));
    code.jg(".loop");

    code.mov(Reg::Rax, Imm::Int64(1));
    code.mov(Reg::Rdi, Imm::Int64(1));
    code.mov(Reg::Rsi, Reg::Rsp);
    code.mov(Reg::Rdx, Reg::Rcx);
    code.syscall();

    code.pop_sf();
    code.ret(4);
}
