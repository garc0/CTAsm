#include <iostream>
#include <ctasm.hpp>

int main() {
    auto bytes = ctasm(R"(
      movdqu xmm0, zword ptr [rdi]
      paddd xmm0, zword ptr [rsi]
      movups zword ptr [rdi], xmm0
      ret
    )");

  for (auto &i : bytes)
    std::cout << std::hex << (int)i << ' ';
  std::cout << '\n';

   return 0; 
}