#include<iostream>
#include <asm_emit.hpp>
#include <asm.hpp>

int main() {

  auto bytes = ctasm( "label1: "
                    " push rcx; "
                    "ret 0; ");
  for (auto &i : bytes)
    std::cout << i << ' ';
  std::cout << '\n';

   return 0; 
}