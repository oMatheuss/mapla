use crate::asm::{self, AsmBuilder, Imm, Mem, MemSize, Reg};
use crate::target::CompilerTarget;

pub fn intrisic(name: &str, code: &mut AsmBuilder, target: CompilerTarget) {
    match name {
        "io" => {
            if let CompilerTarget::Windows = target {
                asm::code!(code, "extern _read, _write");
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
    asm::code!(code, "printChar:");
    asm::code!(code, Push, Reg::Rbp);
    asm::code!(code, Mov, Reg::Rbp, Reg::Rsp);

    let r_01 = match target {
        CompilerTarget::Linux => Reg::Edi,
        CompilerTarget::Windows => Reg::Ecx,
    };
    let p_01 = Mem::offset(Reg::Rbp, -4, MemSize::DWord);

    asm::code!(code, Sub, Reg::Rsp, Imm::Int64(4));
    asm::code!(code, Mov, p_01, r_01);

    match target {
        CompilerTarget::Linux => {
            asm::code!(code, Mov, Reg::Eax, Imm::Int32(1)); // sys_write call
            asm::code!(code, Mov, Reg::Edi, Imm::Int32(1)); // file descriptor
            asm::code!(code, Lea, Reg::Rsi, p_01); // *buffer
            asm::code!(code, Mov, Reg::Edx, Imm::Int32(4)); // count
            asm::code!(code, Syscall);
        }
        CompilerTarget::Windows => {
            asm::code!(code, Mov, Reg::Ecx, Imm::Int32(1)); // file descriptor
            asm::code!(code, Lea, Reg::Rdx, p_01); // *buffer
            asm::code!(code, Mov, Reg::R8D, Imm::Int32(4)); // count
            asm::code!(code, Sub, Reg::Rsp, Imm::Int64(32)); // shadow space
            asm::code!(code, Call, "_write");
        }
    }

    asm::code!(code, Mov, Reg::Rsp, Reg::Rbp);
    asm::code!(code, Pop, Reg::Rbp);
    asm::code!(code, Ret);
}

fn print_int(code: &mut AsmBuilder, target: CompilerTarget) {
    asm::code!(code, "printInt:");
    asm::code!(code, Push, Reg::Rbp);
    asm::code!(code, Mov, Reg::Rbp, Reg::Rsp);

    let p_01 = match target {
        CompilerTarget::Linux => Reg::Edi,
        CompilerTarget::Windows => Reg::Ecx,
    };

    asm::code!(code, Mov, Reg::Eax, p_01); // quotient
    asm::code!(code, Mov, Reg::Ebx, Imm::Int32(10)); // divisor
    asm::code!(code, Mov, Reg::Edx, Imm::Int32(0)); // remainder

    asm::code!(code, Mov, Reg::Rcx, Imm::Int64(0)); // counter

    asm::code!(code, ".L1:");

    asm::code!(code, Xor, Reg::Edx, Reg::Edx);
    asm::code!(code, Idiv, Reg::Ebx);

    asm::code!(code, Inc, Reg::Rcx);

    asm::code!(code, Sub, Reg::Rsp, Imm::Int64(1));
    asm::code!(code, Mov, Mem::reg(Reg::Rsp, MemSize::Byte), Reg::Dl);
    asm::code!(code, Add, Mem::reg(Reg::Rsp, MemSize::Byte), Imm::Char('0'));

    asm::code!(code, Cmp, Reg::Eax, Imm::Int32(0));
    asm::code!(code, Jg, ".L1");

    match target {
        CompilerTarget::Linux => {
            asm::code!(code, Mov, Reg::Eax, Imm::Int32(1)); // sys_write call
            asm::code!(code, Mov, Reg::Edi, Imm::Int32(1)); // file descriptor
            asm::code!(code, Lea, Reg::Rsi, Mem::reg(Reg::Rsp, MemSize::QWord)); // *buffer
            asm::code!(code, Mov, Reg::Edx, Reg::Ecx); // count
            asm::code!(code, Syscall);
        }
        CompilerTarget::Windows => {
            asm::code!(code, Mov, Reg::R8D, Reg::Ecx); // count
            asm::code!(code, Mov, Reg::Ecx, Imm::Int32(1)); // file descriptor
            asm::code!(code, Lea, Reg::Rdx, Mem::reg(Reg::Rsp, MemSize::QWord)); // *buffer
            asm::code!(code, Sub, Reg::Rsp, Imm::Int64(32)); // shadow space
            asm::code!(code, Call, "_write");
        }
    }

    asm::code!(code, Mov, Reg::Rsp, Reg::Rbp);
    asm::code!(code, Pop, Reg::Rbp);
    asm::code!(code, Ret);
}

fn print_string(code: &mut AsmBuilder, target: CompilerTarget) {
    asm::code!(code, "printString:");
    asm::code!(code, Push, Reg::Rbp);
    asm::code!(code, Mov, Reg::Rbp, Reg::Rsp);

    let r_01 = match target {
        CompilerTarget::Linux => Reg::Rdi,
        CompilerTarget::Windows => Reg::Rcx,
    };
    let p_01 = Mem::offset(Reg::Rbp, -8, MemSize::QWord);

    let r_02 = match target {
        CompilerTarget::Linux => Reg::Esi,
        CompilerTarget::Windows => Reg::Edx,
    };
    let p_02 = Mem::offset(Reg::Rbp, -12, MemSize::DWord);

    asm::code!(code, Sub, Reg::Rsp, Imm::Int64(12));
    asm::code!(code, Mov, p_01, r_01);
    asm::code!(code, Mov, p_02, r_02);

    match target {
        CompilerTarget::Linux => {
            asm::code!(code, Mov, Reg::Eax, Imm::Int32(1)); // sys_read call
            asm::code!(code, Mov, Reg::Edi, Imm::Int32(1)); // file descriptor
            asm::code!(code, Mov, Reg::Rsi, p_01); // *buffer
            asm::code!(code, Mov, Reg::Edx, p_02); // count
            asm::code!(code, Syscall);
        }
        CompilerTarget::Windows => {
            asm::code!(code, Mov, Reg::Ecx, Imm::Int32(1)); // file descriptor
            asm::code!(code, Mov, Reg::Rdx, p_01); // *buffer
            asm::code!(code, Mov, Reg::R8D, p_02); // buffer_size
            asm::code!(code, Sub, Reg::Rsp, Imm::Int64(32));
            asm::code!(code, Call, "_write");
        }
    }

    asm::code!(code, Mov, Reg::Rsp, Reg::Rbp);
    asm::code!(code, Pop, Reg::Rbp);
    asm::code!(code, Ret);
}

fn read_string(code: &mut AsmBuilder, target: CompilerTarget) {
    asm::code!(code, "readString:");
    asm::code!(code, Push, Reg::Rbp);
    asm::code!(code, Mov, Reg::Rbp, Reg::Rsp);

    let r_01 = match target {
        CompilerTarget::Linux => Reg::Rdi,
        CompilerTarget::Windows => Reg::Rcx,
    };
    let p_01 = Mem::offset(Reg::Rbp, -8, MemSize::QWord);

    let r_02 = match target {
        CompilerTarget::Linux => Reg::Esi,
        CompilerTarget::Windows => Reg::Edx,
    };
    let p_02 = Mem::offset(Reg::Rbp, -12, MemSize::DWord);

    asm::code!(code, Sub, Reg::Rsp, Imm::Int64(12));
    asm::code!(code, Mov, p_01, r_01);
    asm::code!(code, Mov, p_02, r_02);

    match target {
        CompilerTarget::Linux => {
            asm::code!(code, Mov, Reg::Eax, Imm::Int32(0)); // sys_read call
            asm::code!(code, Mov, Reg::Edi, Imm::Int32(1)); // file descriptor
            asm::code!(code, Mov, Reg::Rsi, p_01); // *buffer
            asm::code!(code, Mov, Reg::Edx, p_02); // count
            asm::code!(code, Syscall);
        }
        CompilerTarget::Windows => {
            asm::code!(code, Mov, Reg::Ecx, Imm::Int32(0)); // file descriptor
            asm::code!(code, Mov, Reg::Rdx, p_01); // *buffer
            asm::code!(code, Mov, Reg::R8D, p_02); // buffer_size
            asm::code!(code, Sub, Reg::Rsp, Imm::Int64(32));
            asm::code!(code, Call, "_read"); // return is already on rax
        }
    }

    asm::code!(code, Mov, Reg::Rsp, Reg::Rbp);
    asm::code!(code, Pop, Reg::Rbp);
    asm::code!(code, Ret);
}
