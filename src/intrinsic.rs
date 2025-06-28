use crate::asm::{AsmBuilder, Imm, Mem, MemSize, Reg};
use crate::target::CompilerTarget;

pub fn intrisic(name: &str, code: &mut AsmBuilder, target: CompilerTarget) {
    match name {
        "io" => {
            if let CompilerTarget::Windows = target {
                code.extrn(&["_read", "_write"]);
            }

            print_int(code, target);
            print_char(code, target);
            print_string(code, target);
            read_string(code, target);
        }
        _ => todo!(),
    }
}

fn print_char(code: &mut AsmBuilder, target: CompilerTarget) {
    code.label("printChar");
    code.push_sf();

    let p_01 = Mem::offset(Reg::Rsp, 16, MemSize::DWord);

    match target {
        CompilerTarget::Linux => {
            code.mov(Reg::Eax, Imm::Int32(1)); // sys_write call
            code.mov(Reg::Edi, Imm::Int32(1)); // file descriptor
            code.lea(Reg::Rsi, p_01); // *buffer
            code.mov(Reg::Edx, Imm::Int32(4)); // count
            code.syscall();
        }
        CompilerTarget::Windows => {
            code.mov(Reg::Ecx, Imm::Int32(1)); // file descriptor
            code.lea(Reg::Rdx, p_01); // *buffer
            code.mov(Reg::R8D, Imm::Int32(4)); // count
            code.sub(Reg::Rsp, Imm::Int64(32)); // shadow space
            code.call("_write");
        }
    }

    code.pop_sf();
    code.ret(4);
}

fn print_int(code: &mut AsmBuilder, target: CompilerTarget) {
    code.label("printInt");
    code.push_sf();

    let p_01 = Mem::offset(Reg::Rbp, 16, MemSize::DWord);

    code.mov(Reg::Rcx, Imm::Int64(0)); // counter

    code.mov(Reg::Eax, p_01); // quotient
    code.mov(Reg::Ebx, Imm::Int32(10)); // divisor
    code.mov(Reg::Edx, Imm::Int32(0)); // remainder

    code.label(".L1");

    code.xor(Reg::Edx, Reg::Edx);
    code.idiv(Reg::Ebx);

    code.inc(Reg::Rcx);

    code.sub(Reg::Rsp, Imm::Int64(1));
    code.mov(Mem::reg(Reg::Rsp, MemSize::Byte), Reg::Dl);
    code.add(Mem::reg(Reg::Rsp, MemSize::Byte), Imm::Char('0'));

    code.cmp(Reg::Eax, Imm::Int32(0));
    code.jg(".L1");

    match target {
        CompilerTarget::Linux => {
            code.mov(Reg::Eax, Imm::Int32(1)); // sys_write call
            code.mov(Reg::Edi, Imm::Int32(1)); // file descriptor
            code.lea(Reg::Rsi, Mem::reg(Reg::Rsp, MemSize::QWord)); // *buffer
            code.mov(Reg::Edx, Reg::Ecx); // count
            code.syscall();
        }
        CompilerTarget::Windows => {
            code.mov(Reg::R8D, Reg::Ecx); // count
            code.mov(Reg::Ecx, Imm::Int32(1)); // file descriptor
            code.lea(Reg::Rdx, Mem::reg(Reg::Rsp, MemSize::QWord)); // *buffer
            code.sub(Reg::Rsp, Imm::Int64(32)); // shadow space
            code.call("_write");
        }
    }

    code.pop_sf();
    code.ret(4);
}

fn print_string(code: &mut AsmBuilder, target: CompilerTarget) {
    code.label("printString");
    code.push_sf();

    let p_01 = Mem::offset(Reg::Rsp, 16, MemSize::QWord);
    let p_02 = Mem::offset(Reg::Rsp, 24, MemSize::DWord);

    match target {
        CompilerTarget::Linux => {
            code.mov(Reg::Eax, Imm::Int32(1)); // sys_write call
            code.mov(Reg::Edi, Imm::Int32(1)); // file descriptor
            code.mov(Reg::Rsi, p_01); // *buffer
            code.mov(Reg::Edx, p_02); // count
            code.syscall();
        }
        CompilerTarget::Windows => {
            code.mov(Reg::Ecx, Imm::Int32(1)); // file descriptor
            code.mov(Reg::Rdx, p_01); // *buffer
            code.mov(Reg::R8D, p_02); // count
            code.sub(Reg::Rsp, Imm::Int64(32)); // shadow space
            code.call("_write");
        }
    }

    code.pop_sf();
    code.ret(12);
}

fn read_string(code: &mut AsmBuilder, target: CompilerTarget) {
    code.label("readString");
    code.push_sf();

    let p_01 = Mem::offset(Reg::Rbp, 16, MemSize::QWord);
    let p_02 = Mem::offset(Reg::Rbp, 24, MemSize::DWord);

    match target {
        CompilerTarget::Linux => {
            code.mov(Reg::Eax, Imm::Int32(0)); // sys_read call
            code.mov(Reg::Edi, Imm::Int32(1)); // file descriptor
            code.mov(Reg::Rsi, p_01); // *buffer
            code.mov(Reg::Edx, p_02); // count
            code.syscall();
        }
        CompilerTarget::Windows => {
            code.mov(Reg::Ecx, Imm::Int32(0)); // file descriptor
            code.mov(Reg::Rdx, p_01); // *buffer
            code.mov(Reg::R8D, p_02); // buffer_size
            code.sub(Reg::Rsp, Imm::Int64(32));
            code.call("_read"); // return is already on rax
        }
    }

    code.pop_sf();
    code.ret(12);
}
