use crate::asm::{AsmBuilder, Imm, Mem, MemSize, Reg};
use crate::target::CompilerTarget;

pub fn intrisic(name: &str, code: &mut AsmBuilder, target: CompilerTarget) {
    match name {
        "io" => {
            match target {
                CompilerTarget::Linux => {
                    linux::read(code);
                    linux::write(code);
                }
                CompilerTarget::Windows => {
                    code._extern(&["_read", "_write"]);
                    windows::read(code);
                    windows::write(code);
                }
                CompilerTarget::Unknown => {}
            };
            print_int(code);
            print_char(code);
            print_string(code);

            read_string(code);
        }
        _ => todo!(),
    }
}

// func write(int fd, char* buffer, int count);
// func read(int fd, char* buffer, int buffer_size): int;

mod windows {
    use crate::asm::{AsmBuilder, Imm, Mem, MemSize, Reg};

    // https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170
    // The caller must always allocate sufficient space to store four register parameters, even if the callee doesn't take that many parameters
    // Any parameters beyond the first four must be stored on the stack after the shadow store before the call.

    pub fn read(code: &mut AsmBuilder) {
        code.label("read");
        code.push_sf();
        code.sub(Reg::Rsp, Imm::Int64(32));
        code.mov(Reg::Ecx, Mem::offset(Reg::Rbp, 16, MemSize::DWord)); // file descriptor
        code.mov(Reg::Rdx, Mem::offset(Reg::Rbp, 20, MemSize::QWord)); // *buffer
        code.mov(Reg::R8D, Mem::offset(Reg::Rbp, 28, MemSize::DWord)); // buffer_size
        code.call("_read"); // return is already on rax
        code.pop_sf();
        code.ret(16);
    }
    pub fn write(code: &mut AsmBuilder) {
        code.label("write");
        code.push_sf();
        code.sub(Reg::Rsp, Imm::Int64(32));
        code.mov(Reg::Ecx, Mem::offset(Reg::Rbp, 16, MemSize::DWord)); // file descriptor
        code.mov(Reg::Rdx, Mem::offset(Reg::Rbp, 20, MemSize::QWord)); // *buffer
        code.mov(Reg::R8D, Mem::offset(Reg::Rbp, 28, MemSize::DWord)); // count
        code.call("_write");
        code.pop_sf();
        code.ret(16);
    }
}

mod linux {
    use crate::asm::{AsmBuilder, Imm, Mem, MemSize, Reg};

    pub fn read(code: &mut AsmBuilder) {}
    pub fn write(code: &mut AsmBuilder) {
        code.label("write");
        code.mov(Reg::Eax, Imm::Int32(1)); // sys_write call
        code.mov(Reg::Edi, Mem::offset(Reg::Rsp, 8, MemSize::DWord)); // file descriptor
        code.mov(Reg::Rsi, Mem::offset(Reg::Rsp, 12, MemSize::QWord)); // *buffer
        code.mov(Reg::Edx, Mem::offset(Reg::Rsp, 20, MemSize::DWord)); // count
        code.syscall();
        code.ret(16);
    }
}

fn print_char(code: &mut AsmBuilder) {
    code.label("printChar");

    code.lea(Reg::Rax, Mem::offset(Reg::Rsp, 8, MemSize::DWord)); // save start of int -> char*

    code.sub(Reg::Rsp, Imm::Int64(16));
    code.mov(Mem::reg(Reg::Rsp, MemSize::DWord), Imm::Int32(1)); // int fd
    code.mov(Mem::offset(Reg::Rsp, 4, MemSize::QWord), Reg::Rax); // char* buffer
    code.mov(Mem::offset(Reg::Rsp, 12, MemSize::DWord), Imm::Int32(4)); // int count
    code.call("write");

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

    code.label(".L1");

    code.xor(Reg::Edx, Reg::Edx);
    code.idiv(Reg::Ebx);

    code.inc(Reg::Rcx);

    code.sub(Reg::Rsp, Imm::Int64(1));
    code.mov(Mem::reg(Reg::Rsp, MemSize::Byte), Reg::Dl);
    code.add(Mem::reg(Reg::Rsp, MemSize::Byte), Imm::Char('0'));

    code.cmp(Reg::Eax, Imm::Int32(0));
    code.jg(".L1");

    code.lea(Reg::Rax, Mem::reg(Reg::Rsp, MemSize::QWord)); // save start of char*

    code.sub(Reg::Rsp, Imm::Int64(16));
    code.mov(Mem::reg(Reg::Rsp, MemSize::DWord), Imm::Int32(1)); // int fd
    code.mov(Mem::offset(Reg::Rsp, 4, MemSize::QWord), Reg::Rax); // char* buffer
    code.mov(Mem::offset(Reg::Rsp, 12, MemSize::DWord), Reg::Ecx); // int count
    code.call("write");

    code.pop_sf();
    code.ret(4);
}

fn print_string(code: &mut AsmBuilder) {
    code.label("printString");

    code.sub(Reg::Rsp, Imm::Int64(16));
    code.mov(Mem::reg(Reg::Rsp, MemSize::DWord), Imm::Int32(1)); // int fd
    code.mov(Reg::Rax, Mem::offset(Reg::Rsp, 24, MemSize::QWord));
    code.mov(Mem::offset(Reg::Rsp, 4, MemSize::QWord), Reg::Rax); // char* buffer
    code.mov(Reg::Eax, Mem::offset(Reg::Rsp, 32, MemSize::DWord));
    code.mov(Mem::offset(Reg::Rsp, 12, MemSize::DWord), Reg::Eax); // int count
    code.call("write");

    code.ret(12);
}

fn read_string(code: &mut AsmBuilder) {
    code.label("readString");

    code.sub(Reg::Rsp, Imm::Int64(16));
    code.mov(Mem::reg(Reg::Rsp, MemSize::DWord), Imm::Int32(0)); // int fd
    code.mov(Reg::Rax, Mem::offset(Reg::Rsp, 24, MemSize::QWord));
    code.mov(Mem::offset(Reg::Rsp, 4, MemSize::QWord), Reg::Rax); // char* buffer
    code.mov(Reg::Eax, Mem::offset(Reg::Rsp, 32, MemSize::DWord));
    code.mov(Mem::offset(Reg::Rsp, 12, MemSize::DWord), Reg::Eax); // int buffer_size
    code.call("read");

    code.ret(12);
}