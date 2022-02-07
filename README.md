# Ctasm -- Compile Time Assembler for C++ (WIP)

## Idea
Ctasm is fully implemented in C++ templates (they are turing complete) and allows you to compile assembly code in NASM syntax from a string, saving the resulting machine code to an array without traces of the source string in the program.
### Features 
- extends: sse, sse2, sse4_2, avx, avx2 etc...

#### Tested on
* clang >= 12.0
* gcc >= 11.1
* msvc >= 19.30

## 
## Example 
``` c++
 auto bytes = ctasm(
        "example: "
        "movdqu  xmm0, zword ptr [rdi];" 
        "paddd   xmm0, zword ptr [rsi];" 
        "movups  zword ptr [rdi], xmm0;"
        "ret 0;"
    );
    
  for (auto &i : bytes)
    std::cout << std::hex << (int)i << ' ';
  std::cout << '\n';
```
## License
Apache2