#include <iostream>
#include <ctasm.hpp>

int main() {
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

   return 0; 
}