
# CTAsm -- Compile Time Assembler for C++ (WIP)
#### Supported compilers
* Clang (std 14-- last)
* GCC (std 14 -- last) 
* Visual C++ (std 17 -- last)

### That's why
* That's can compile your assembler code while you compile your main code 
* I tired of seeing raw data implementations(for example injectors) of assembler code or split my 64bit code to diff files
* Because I can and it works

### What work for now:
* *full* number of registers(8 -- 64 + extend)
* AVX & other extends
### How to basic use it to compile out x64 code 
``` c++
 auto bytes = ctasm("label1: "
                    "push rcx; "
                    "ret 0; ");
  for (auto &i : bytes)
    std::cout << i << ' ';
  std::cout << '\n';
```
Variable "bytes" would hold all compiled data, which you can use later. 
All string of assembler code will disappear in binary
