#ifndef CTASM_HPP
#define CTASM_HPP

#pragma once

#include <limits>
#include <iostream>
#include <string>
#include <string_view>
#include <functional>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>
#include <array>
#include <algorithm>
#include <string_view>

namespace ctasm
{

template <uint8_t... chars> using byte_seq = std::integer_sequence<uint8_t, chars...>;
template <char...  chars> using char_seq = std::integer_sequence<char,  chars...>;

template <typename T> using null_sequence = std::integer_sequence<T>;

template <typename... T>
struct expand_byte_seq {
  using type = std::integer_sequence<uint8_t>;
};

template <uint8_t... a>
struct expand_byte_seq<std::integer_sequence<uint8_t, a...>> {
  using type = std::integer_sequence<uint8_t, a...>;
};

template <uint8_t... a, uint8_t... a1>
struct expand_byte_seq<std::integer_sequence<uint8_t, a...>,
                       std::integer_sequence<uint8_t, a1...>> {
  using type = std::integer_sequence<uint8_t, a..., a1...>;
};

template <uint8_t... a, uint8_t... a1, typename... T>
struct expand_byte_seq<std::integer_sequence<uint8_t, a...>,
                       std::integer_sequence<uint8_t, a1...>, T...> {
  using type =
      typename expand_byte_seq<std::integer_sequence<uint8_t, a..., a1...>,
                               T...>::type;
};

template<typename ...T>
using expand_byte_seq_v = typename expand_byte_seq<T...>::type;

template<typename ...T>
using expand_byte_seq_t = typename expand_byte_seq<T...>::type;

template <typename>
struct seq_to_arr {};

template <uint8_t... ints>
struct seq_to_arr<std::integer_sequence<uint8_t, ints...>> {
  static constexpr auto value = std::array<uint8_t, sizeof...(ints)>{ints...};
};
template <char... ints>
struct seq_to_arr<std::integer_sequence<char, ints...>> {
  static constexpr auto value = std::array<char, sizeof...(ints)>{ints...};
};

template <typename... T> struct type_list {};

template <typename Head, typename... T> 
struct type_list<Head, T...> {
  using head = Head;
  using tail = type_list<T...>;

  static constexpr std::size_t sz = sizeof...(T) + 1;
};

template <typename Head> 
struct type_list<Head> {
  using head = Head;
  using tail = type_list<>;

  static constexpr std::size_t sz = 1;
};

template <>
struct type_list<> {
  using head = void;
  using tail = type_list<>;

  static constexpr std::size_t sz = 0;
};

template<typename ...T>
struct type_list_append {};

template <typename... T>
struct type_list_append<type_list<T...>> {
  using type = type_list<T...>;
};

template <typename... T>
struct type_list_append<type_list<T...>, type_list<>> {
  using type = type_list<T...>;
};

template <typename ...T, typename ...Y>
struct type_list_append<type_list<T...>, type_list<Y...>> {
    using type = type_list<T..., Y...>;
};

template <typename T, typename... Y>
struct type_list_append<T, Y...> {
  using type = typename type_list_append<T, typename type_list_append<Y...>::type>::type;
};

template<typename ...T>
using type_list_append_t = typename type_list_append<T...>::type;

template <typename... T> using hold = type_list<T...>;


template <class... Args1> struct zip {
  template <class... Args2> struct with { using type = hold<hold<Args1...>, Args2...>; };
};

struct sreg {};
struct creg {};
struct dreg {};

struct reg8 {};
struct reg16 {};
struct reg32 {};
struct reg64 {};
struct reg80 {};
struct reg128 {};
struct reg256 {};
struct reg512 {};
struct reg1024 {};

struct mm_reg {};
struct mmx {};

// mask reg
struct reg_k {};

template <uint8_t N> 
struct reg : std::integral_constant<uint8_t, N % 8> {};

template <typename... T> 
struct reg_n { 
  static constexpr uint8_t value = 0; 
};

template <uint8_t N>
struct reg_n<reg<N>> : reg<N>{};

template <uint8_t N, typename... T> 
struct reg_n<reg<N>, T...> : reg<N> {};

template <uint8_t N, typename... T> 
struct reg_n<hold<reg<N>, T...>> : reg<N> {};

template <uint8_t N, typename... T, typename... Y>
struct reg_n<hold<Y...>, reg<N>, T...> : reg<N> {};

struct ext {};
struct avx_ext {};

typedef zip<sreg>::with<reg<0>> es;
typedef zip<sreg>::with<reg<1>> cs;
typedef zip<sreg>::with<reg<2>> ss;
typedef zip<sreg>::with<reg<3>> ds;
typedef zip<sreg>::with<reg<4>> fs;
typedef zip<sreg>::with<reg<5>> gs;

typedef zip<dreg>::with<reg<0>> db0;
typedef zip<dreg>::with<reg<1>> db1;
typedef zip<dreg>::with<reg<2>> db2;
typedef zip<dreg>::with<reg<3>> db3;
typedef zip<dreg>::with<reg<4>> db6;
typedef zip<dreg>::with<reg<5>> db7;

typedef zip<creg>::with<reg<0>> cr0;
typedef zip<creg>::with<reg<1>> cr2;
typedef zip<creg>::with<reg<2>> cr3;
typedef zip<creg>::with<reg<3>> cr4;

typedef zip<reg8>::with<reg<0>> al;
typedef zip<reg8>::with<reg<1>> cl;
typedef zip<reg8>::with<reg<2>> dl;
typedef zip<reg8>::with<reg<3>> bl;
typedef zip<reg8>::with<reg<4>> ah;
typedef zip<reg8>::with<reg<5>> ch;
typedef zip<reg8>::with<reg<6>> dh;
typedef zip<reg8>::with<reg<7>> bh;

typedef zip<reg8>::with<reg<0>, ext> r8l;
typedef zip<reg8>::with<reg<1>, ext> r9l;
typedef zip<reg8>::with<reg<2>, ext> r10l;
typedef zip<reg8>::with<reg<3>, ext> r11l;
typedef zip<reg8>::with<reg<4>, ext> r12l;
typedef zip<reg8>::with<reg<5>, ext> r13l;
typedef zip<reg8>::with<reg<6>, ext> r14l;
typedef zip<reg8>::with<reg<7>, ext> r15l;


typedef zip<reg16>::with<reg<0>> ax;
typedef zip<reg16>::with<reg<3>> bx;
typedef zip<reg16>::with<reg<1>> cx;
typedef zip<reg16>::with<reg<2>> dx;
typedef zip<reg16>::with<reg<6>> si;
typedef zip<reg16>::with<reg<7>> di;
typedef zip<reg16>::with<reg<4>> sp;
typedef zip<reg32>::with<reg<5>> bp;

typedef zip<reg16>::with<reg<0>, ext> r8w;
typedef zip<reg16>::with<reg<1>, ext> r9w;
typedef zip<reg16>::with<reg<2>, ext> r10w;
typedef zip<reg16>::with<reg<3>, ext> r11w;
typedef zip<reg16>::with<reg<4>, ext> r12w;
typedef zip<reg16>::with<reg<5>, ext> r13w;
typedef zip<reg16>::with<reg<6>, ext> r14w;
typedef zip<reg16>::with<reg<7>, ext> r15w;

typedef zip<reg32>::with<reg<0>> eax;
typedef zip<reg32>::with<reg<3>> ebx;
typedef zip<reg32>::with<reg<1>> ecx;
typedef zip<reg32>::with<reg<2>> edx;
typedef zip<reg32>::with<reg<6>> esi;
typedef zip<reg32>::with<reg<7>> edi;
typedef zip<reg32>::with<reg<4>> esp;
typedef zip<reg32>::with<reg<5>> ebp;


typedef zip<reg32>::with<reg<0>> zax;
typedef zip<reg32>::with<reg<3>> zbx;
typedef zip<reg32>::with<reg<1>> zcx;
typedef zip<reg32>::with<reg<2>> zdx;
typedef zip<reg32>::with<reg<6>> zsi;
typedef zip<reg32>::with<reg<7>> zdi;
typedef zip<reg32>::with<reg<4>> zsp;
typedef zip<reg32>::with<reg<5>> zbp;

typedef zip<reg32>::with<reg<0>, ext> r8d;
typedef zip<reg32>::with<reg<1>, ext> r9d;
typedef zip<reg32>::with<reg<2>, ext> r10d;
typedef zip<reg32>::with<reg<3>, ext> r11d;
typedef zip<reg32>::with<reg<4>, ext> r12d;
typedef zip<reg32>::with<reg<5>, ext> r13d;
typedef zip<reg32>::with<reg<6>, ext> r14d;
typedef zip<reg32>::with<reg<7>, ext> r15d;

typedef zip<reg64>::with<reg<0>> rax;
typedef zip<reg64>::with<reg<3>> rbx;
typedef zip<reg64>::with<reg<1>> rcx;
typedef zip<reg64>::with<reg<2>> rdx;
typedef zip<reg64>::with<reg<6>> rsi;
typedef zip<reg64>::with<reg<7>> rdi;
typedef zip<reg64>::with<reg<4>> rsp;
typedef zip<reg64>::with<reg<5>> rbp;

typedef zip<reg64>::with<reg<5>> rip;

typedef zip<reg64>::with<reg<0>, ext> r8;
typedef zip<reg64>::with<reg<1>, ext> r9;
typedef zip<reg64>::with<reg<2>, ext> r10;
typedef zip<reg64>::with<reg<3>, ext> r11;
typedef zip<reg64>::with<reg<4>, ext> r12;
typedef zip<reg64>::with<reg<5>, ext> r13;
typedef zip<reg64>::with<reg<6>, ext> r14;
typedef zip<reg64>::with<reg<7>, ext> r15;

template <uint8_t N>
using mm = typename std::conditional_t<
    N / 8 == 0, zip<mmx>::with<reg<N % 8>>,
    typename std::conditional_t<N / 8 == 1, zip<mmx>::with<reg<N % 8>, ext>,
                                void>>;

template <uint8_t N>
using st = typename std::conditional_t<
    N / 8 == 0, zip<reg80>::with<reg<N % 8>>,
    typename std::conditional_t<N / 8 == 1, zip<reg80>::with<reg<N % 8>, ext>,
                                void>>;

template<class T, uint8_t N>
using _mm = 
  typename std::conditional_t<N / 8 == 0, typename T::template with<reg<N % 8>>,
  typename std::conditional_t<N / 8 == 1, typename T::template with<reg<N % 8>, ext>,
  typename std::conditional_t<N / 8 == 2, typename T::template with<reg<N % 8>, avx_ext>,
  typename std::conditional_t<N / 8 == 3, typename T::template with<reg<N % 8>, ext, avx_ext>,
                void>>>>;

template <uint8_t N> using xmm = _mm<zip<reg128>, N>;
template <uint8_t N> using ymm = _mm<zip<reg256>, N>;
template <uint8_t N> using zmm = _mm<zip<reg512>, N>;
template <uint8_t N> using tmm = _mm<zip<reg80>, N>;

struct zmask {};

typedef zip<reg_k>::with<reg<0>>::type k0;
typedef zip<reg_k>::with<reg<1>>::type k1;
typedef zip<reg_k>::with<reg<2>>::type k2;
typedef zip<reg_k>::with<reg<3>>::type k3;
typedef zip<reg_k>::with<reg<4>>::type k4;
typedef zip<reg_k>::with<reg<5>>::type k5;
typedef zip<reg_k>::with<reg<6>>::type k6;
typedef zip<reg_k>::with<reg<7>>::type k7;

template <typename... T> struct ptr {};

template<typename T>
using get_reg_sz = std::conditional_t<std::is_same<T, reg8>::value, std::integral_constant<uint8_t, 1>,
                   std::conditional_t<std::is_same<T, reg16>::value, std::integral_constant<uint8_t, 2>, 
                   std::conditional_t<std::is_same<T, reg32>::value, std::integral_constant<uint8_t, 4>,
                   std::conditional_t<std::is_same<T, reg64>::value, std::integral_constant<uint8_t, 8>,
                   std::conditional_t<std::is_same<T, reg128>::value, std::integral_constant<uint8_t, 16>, void>
                   >>>>;

template<std::size_t Sz, std::uint64_t N>
struct ux{
  using value = expand_byte_seq_t<typename ux<Sz - 1, N>::value, byte_seq<(uint8_t(N >> (8 * (Sz - 1))) & 0xFF)>>;
};

template<std::uint64_t N>
struct ux<0, N>{
  using value = byte_seq<>;
};

template<uint8_t  D> using ub = ux<1, D>;
template<uint16_t D> using uw = ux<2, D>;
template<uint32_t D> using ud = ux<4, D>;
template<uint64_t D> using uq = ux<8, D>;

template <uint8_t N>  using rel8  = ub<N>;
template <uint16_t N> using rel16 = uw<N>;
template <uint32_t N> using rel32 = ud<N>;
template <uint64_t N> using rel64 = uq<N>;

template <uint8_t N>  using disp8  = ub<N>;
template <uint16_t N> using disp16 = uw<N>;
template <uint32_t N> using disp32 = ud<N>;
template <uint64_t N> using disp64 = uq<N>;


template <int8_t N>  struct ib : ub<static_cast<uint8_t>(N)> {};
template <int16_t N> struct iw : uw<static_cast<uint16_t>(N)> {};
template <int32_t N> struct id : ud<static_cast<uint32_t>(N)> {};
template <int64_t N> struct iq : uq<static_cast<uint64_t>(N)> {};

template <typename... T> struct disp_reg {};

template <typename... T, uint8_t N>  struct disp_reg<hold<T...>, ub<N>> : ub<N> {};
template <typename... T, uint16_t N> struct disp_reg<hold<T...>, uw<N>> : uw<N> {};
template <typename... T, uint32_t N> struct disp_reg<hold<T...>, ud<N>> : ud<N> {};
template <typename... T, uint64_t N> struct disp_reg<hold<T...>, uq<N>> : uq<N> {};

template<typename  > struct unpack_value;
template<uint8_t  D> struct unpack_value<ub<D>>: std::integral_constant<uint8_t,  D>{};
template<uint16_t D> struct unpack_value<uw<D>>: std::integral_constant<uint16_t, D>{};
template<uint32_t D> struct unpack_value<ud<D>>: std::integral_constant<uint32_t, D>{};
template<uint64_t D> struct unpack_value<uq<D>>: std::integral_constant<uint64_t, D>{};

template<typename Ty, class Enable = bool>
struct u64_8{};

template <class T>
struct u64_8<T, 
          std::enable_if_t<sizeof(unpack_value<T>::value) <= 8, bool>> {
  using type = bool;
  using value = 
      typename uq<static_cast<uint64_t>(unpack_value<T>::value)>::value;
};

template <typename Ty, class Enable = bool>
struct u32_8{};

template <class T>
struct u32_8<T, std::enable_if_t<sizeof(unpack_value<T>::value) <= 4, bool>> {
  using type = bool;
  using value =
      typename ud<static_cast<uint32_t>(unpack_value<T>::value)>::value;
};

template <typename Ty, class Enable = bool>
struct u16_8{
  using type = void;
};

template <class T>
struct u16_8<T, std::enable_if_t<sizeof(unpack_value<T>::value) <= 2, bool>> {
  using type = bool;
  using value =
      typename uw<static_cast<uint16_t>(unpack_value<T>::value)>::value;
};

template<typename Ty>
struct u8_8{};

template <uint8_t V>
struct u8_8<ub<V>> {
  using type = bool;
  using value = typename ub<V>::value;
};

template<typename Ty, class Enable = bool>
struct i64_8{};

template <class T>
struct i64_8<T, 
          std::enable_if_t<sizeof(unpack_value<T>::value) <= 8, bool>> {
  using type = bool;
  using value = 
      typename iq<static_cast<int64_t>(unpack_value<T>::value)>::value;
};

template <typename Ty, class Enable = bool>
struct i32_8{};

template <class T>
struct i32_8<T, std::enable_if_t<sizeof(unpack_value<T>::value) <= 4, bool>> {
  using type = bool;
  using value =
      typename id<static_cast<int32_t>(unpack_value<T>::value)>::value;
};

template <typename Ty, class Enable = bool>
struct i16_8{};

template <class T>
struct i16_8<T, std::enable_if_t<sizeof(unpack_value<T>::value) <= 2, bool>> {
  using type = bool;
  using value =
      typename iw<static_cast<int16_t>(unpack_value<T>::value)>::value;
};

template<typename Ty>
struct i8_8{};

template <int8_t V>
struct i8_8<ib<V>> {
  using type = bool;
  using value = typename ib<V>::value;
};

template<typename> struct unpack_hold;
template<class T, class ...Y> struct unpack_hold<hold<T, Y...>>{
  using value = expand_byte_seq_v<typename T::value, typename unpack_hold<hold<Y...>>::value>;
};

template<class T> struct unpack_hold<hold<T>>{
  using value = typename T::value;
};

template<std::size_t N, class T>
struct dup{
  using value = expand_byte_seq_v<typename unpack_hold<T>::value, typename dup<N - 1, T>::value>;
};

template<class T>
struct dup<0, T>{
  using value = byte_seq<>;
};

template<uint64_t N>
struct _log2{
  static constexpr uint64_t value = 1 + _log2<N / 2>::value;
};

template<>
struct _log2<1>{
  static constexpr uint64_t value = 0; 
};

template<>
struct _log2<0>{
  static constexpr uint64_t value = 0; 
};

template <typename... T> struct is_ext: std::false_type {};
template <uint8_t N, typename... T> struct is_ext<reg<N>, ext, T...> : std::true_type {};
template <typename... T> struct is_ext<hold<T...>> : is_ext<T...> {};

template<typename... T>
static constexpr auto is_ext_v = is_ext<T...>::value;

template <typename... T> struct is_avx_ext: std::false_type {};
template <uint8_t N, typename... T> struct is_avx_ext<reg<N>, avx_ext, T...> : std::true_type {};
template <uint8_t N, typename... T> struct is_avx_ext<reg<N>, ext, avx_ext, T...> : std::true_type {};
template <typename... T> struct is_avx_ext<hold<T...>>: is_avx_ext<T...> {};

template<typename... T>
static constexpr auto is_avx_ext_v = is_avx_ext<T...>::value;

template <uint8_t W, uint8_t R, uint8_t X, uint8_t B> struct REX {
  using value = byte_seq<B | (X << 1) | (R << 2) | (W << 3) | (0b0100 << 4)>;
};

template <> struct REX<0, 0, 0, 0> {
  using value = std::integer_sequence<uint8_t>;
};

template <typename... T> struct XOP {};

template <uint8_t R, uint8_t X, uint8_t B, uint8_t m, uint8_t W, typename... T,
          uint16_t length, uint8_t pp>
struct XOP<ub<R>, ub<X>, ub<B>, ub<m>, ub<W>, hold<T...>,
           uw<length>, ub<pp>> {
  static constexpr auto vvvv = reg_n<hold<T...>>::value | (is_ext_v<hold<T...>> << 3);

  using value = byte_seq<
      0x8f, 
      uint8_t((~R & 1) << 7 | (~X & 1) << 6 | (~B & 1) << 5 | m),
      uint8_t((W & 1) << 7 | (~vvvv & 0b1111) << 3 | ((length == 256) & 1) << 2 | (pp & 3))>;
};

template <typename... T> struct VEX {};

template <uint8_t R, typename... T, uint16_t length, uint8_t pp_byte>
struct VEX<ub<R>, hold<T...>, uw<length>, ub<pp_byte>> {
  static constexpr uint8_t pp = pp_byte == 0x66   ? 1
                                : pp_byte == 0xF3 ? 2
                                : pp_byte == 0xF2 ? 3
                                                  : 0;

  static constexpr auto vvvv =
      reg_n<hold<T...>>::value | (is_ext_v<hold<T...>> << 3);

  using value = byte_seq<
      0b11000101, uint8_t((~R & 1) << 7 | (~vvvv & 0b1111) << 3 |
                          ((length == 256) & 1) << 2 | (pp & 3))>;
};

template <uint8_t R, uint8_t X, uint8_t B, uint8_t m, uint8_t W, typename... T,
          uint16_t length, uint8_t pp_byte>
struct VEX<ub<R>, ub<X>, ub<B>, ub<m>, ub<W>, hold<T...>,
           uw<length>, ub<pp_byte>> {
  static constexpr uint8_t pp = pp_byte == 0x66   ? 1
                                : pp_byte == 0xF3 ? 2
                                : pp_byte == 0xF2 ? 3
                                                  : 0;

  static constexpr auto vvvv =
      reg_n<T...>::value | (is_ext_v<hold<T...>> << 3);

  using value = byte_seq<
      0b11000100, uint8_t((~R & 1) << 7 | (~X & 1) << 6 | (~B & 1) << 5 | m),
      uint8_t((W & 1) << 7 | (~vvvv & 0b1111) << 3 |
              ((length == 256) & 1) << 2 | (pp & 3))>;
};

template <typename... T> struct EVEX {};

template <uint8_t R, uint8_t X, uint8_t B, uint8_t R1, uint8_t m, uint8_t W,
          typename... T, uint8_t pp_byte, uint8_t z, uint16_t length, uint8_t b,
          typename... Y>

struct EVEX<ub<R>, ub<X>, ub<B>, ub<R1>, ub<m>, ub<W>,
            hold<T...>, ub<pp_byte>, ub<z>, uw<length>, ub<b>,
            hold<Y...>> {

  static constexpr uint8_t pp = pp_byte == 0x66   ? 1
                                : pp_byte == 0xF3 ? 2
                                : pp_byte == 0xF2 ? 3
                                                  : 0;

  static constexpr auto vvvv =
      reg_n<hold<T...>>::value | (is_ext_v<hold<T...>> << 3);
  static constexpr auto V1 = is_avx_ext_v<T...>;
  static constexpr auto a = reg_n<T...>::value;
  static constexpr auto L1 = length == 512;
  static constexpr auto L = length == 256;

  using value = byte_seq<
      0x62,
      uint8_t((m & 0x3) | ((~R1 & 1) << 4) | ((~B & 1) << 5) | ((~X & 1) << 6) |
              ((~R & 1) << 7)),
      uint8_t((pp & 0b11) | (1 << 2) | ((~vvvv & 0b1111) << 3) |
              ((W & 1) << 7)),
      uint8_t(a | (~V1 & 1) << 3 | (b & 1) << 4 | (L & 1) << 5 | (L1 & 1) << 6 |
              z << 7)>;
};

template <typename... T> struct SIB {};

template <typename... T, typename... Y, uint8_t scale>
struct SIB<disp8<scale>, hold<T...>, hold<Y...>> {
  static const uint8_t index = reg_n<T...>::value;
  static const uint8_t base = reg_n<Y...>::value;
  using value = byte_seq<uint8_t(
      (_log2<scale & 0xFF>::value << 6) | ((index & 0xFF) << 3) | (base & 0xFF))>;
};

template <typename... T> struct disp_SIB {};

template <uint8_t scale, uint8_t D, typename... T, typename... Y>
struct disp_SIB<disp8<scale>, disp8<D>, hold<T...>, hold<Y...>> {
  static const uint8_t index = reg_n<T...>::value;
  static const uint8_t base = reg_n<Y...>::value;
  using value = expand_byte_seq_v<
      byte_seq<uint8_t((_log2<scale & 0xFF>::value << 6) | (index << 3) | (base))>, typename disp8<D>::value>;
};

template <uint8_t scale, uint32_t D, typename... T, typename... Y>
struct disp_SIB<disp8<scale>, disp32<D>, hold<T...>, hold<Y...>> {
  static const uint8_t index = reg_n<T...>::value;
  static const uint8_t base = reg_n<Y...>::value;
  using value = expand_byte_seq_v<byte_seq<uint8_t((_log2<scale & 0xFF>::value << 6) | (index << 3) | (base))>,
      typename disp32<D>::value>;
};

template <typename... T> struct modrm {
  static const uint8_t mod = 0;
  static const uint8_t rop = 0;
  static const uint8_t rm = 0;
  using value = std::integer_sequence<uint8_t>;
};

template <typename... T, typename... Y> struct modrm<hold<T...>, hold<Y...>> {
  static const uint8_t mod = 0b11;
  static const uint8_t rop = reg_n<Y...>::value;
  static const uint8_t rm = reg_n<T...>::value;

  using value = byte_seq<(mod << 6) | (rm) | ((rop) << 3)>;
};

template <typename... T, typename... Y>
struct modrm<ptr<hold<T...>>, hold<Y...>> {
  static const uint8_t mod = 0b00;
  static const uint8_t rop = reg_n<Y...>::value;
  static const uint8_t rm = reg_n<T...>::value;

  using value = byte_seq<uint8_t((mod << 6) | (rm) | (rop << 3))>;
};

template <typename... T, uint8_t N, typename... Y>
struct modrm<disp_reg<hold<T...>, disp8<N>>, hold<Y...>> {
  static const uint8_t mod = 0b01;
  static const uint8_t rop = reg_n<Y...>::value;
  static const uint8_t rm = reg_n<T...>::value;

  using value = expand_byte_seq_v<byte_seq<((mod << 6) | (rm) | (rop << 3))>,
                        typename disp_reg<hold<T...>, disp8<N>>::value>;
};

template <typename... T, uint32_t N, typename... Y>
struct modrm<disp_reg<hold<T...>, disp32<N>>, hold<Y...>> {
  static const uint8_t mod = 0b10;
  static const uint8_t rop = reg_n<Y...>::value;
  static const uint8_t rm = reg_n<T...>::value;

  using value = expand_byte_seq_v<byte_seq<((mod << 6) | (rm) | (rop << 3))>,
                        typename disp_reg<hold<T...>,  disp32<N>>::value>;
};

template <uint8_t Scale, typename... T, typename... Y, typename... Z>
struct modrm<SIB<disp8<Scale>, hold<T...>, hold<Y...>>, hold<Z...>> {
  static const uint8_t mod = 0b00;
  static const uint8_t rop = reg_n<Z...>::value;
  static const uint8_t rm = 0b100;

  using value = expand_byte_seq_v<byte_seq<((mod << 6) | (rm) | (rop << 3))>,
      typename SIB<disp8<Scale>, hold<T...>, hold<Y...>>::value>;
};

template <uint8_t Scale, uint8_t D, typename... T, typename... Y, typename... Z>
struct modrm<disp_SIB<disp8<Scale>, disp8<D>, hold<T...>, hold<Y...>>,
             hold<Z...>> {
  static const uint8_t mod = 0b01;
  static const uint8_t rop = reg_n<Z...>::value;
  static const uint8_t rm = 0b100;

  using value = expand_byte_seq_v<byte_seq<((mod << 6) | (rm) | (rop << 3))>,
      typename disp_SIB<disp8<Scale>, disp8<D>, hold<T...>, hold<Y...>>::value>;
};

template <uint8_t Scale, uint32_t D, typename... T, typename... Y,
          typename... Z>
struct modrm<disp_SIB<disp8<Scale>, disp32<D>, hold<T...>, hold<Y...>>,
             hold<Z...>> {
  static const uint8_t mod = 0b10;
  static const uint8_t rop = reg_n<Z...>::value;
  static const uint8_t rm = 0b100;

  using value = expand_byte_seq_v<byte_seq<((mod << 6) | (rm) | (rop << 3))>,
      typename disp_SIB<disp8<Scale>, disp32<D>, hold<T...>, hold<Y...>>::value>;
};

struct plus {};
struct mul {};

using regs_bit = reg64;

template <typename... T> struct mrm {
  static constexpr auto R = 0;
  static constexpr auto X = 0;
  static constexpr auto B = 0;

  using _67h = std::false_type;
};

template <typename... T>
struct extract_args{};

template <typename T, typename... Y>
struct extract_args<hold<hold<T>, Y...>> {
    using value = hold<Y...>;
};

template<typename ...T>
using extract_args_v = typename extract_args<T...>::value;

template <typename... T>
struct extract_head {};

template <typename T, typename... Y>
struct extract_head<hold<hold<T>, Y...>> {
  using value = T;
};

template<typename ...T>
using extract_head_v = typename extract_head<T...>::value;

template <typename T>
using _is_67h_needed = 
        typename std::conditional_t<std::is_same<T, reg32>::value, std::is_same<regs_bit, reg64>,
        typename std::conditional_t<std::is_same<T, reg16>::value, std::is_same<regs_bit, reg32>,
        typename std::conditional_t<std::is_same<T, reg8>::value,  std::is_same<regs_bit, reg16>,
                       std::false_type>>>::type;

template <typename T>
using is_67h_needed = std::conditional_t<std::is_same<_is_67h_needed<T>, std::false_type>::value, std::integer_sequence<uint8_t>, byte_seq<0x67>>;

template <class Y>
struct mrm<Y> {
    template <typename T>
    using value = typename
      modrm<ptr<extract_args_v<typename Y::type>>, T>::value;

    static constexpr auto R = 0;
    static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
    static constexpr auto B = 0;

    using _67h = is_67h_needed<extract_head_v<typename Y::type>>;
};

template <class Y, uint8_t N>
struct mrm<Y, plus, disp8<N>> {
    template <typename T>
    using value = typename 
      modrm<disp_reg<extract_args_v<typename Y::type>, disp8<N>>,
            T>::value;

    static constexpr auto R = 0;
    static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
    static constexpr auto B = 0;

    using _67h = is_67h_needed<extract_head_v<typename Y::type>>;
};

template <class Y, uint32_t N>
struct mrm<Y, plus, disp32<N>> {
    template <typename T>
    using value = typename 
      modrm<disp_reg<extract_args_v<typename Y::type>, disp32<N>>,
            T>::value;

    static constexpr auto R = 0;
    static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
    static constexpr auto B = 0;

    using _67h = is_67h_needed<extract_head_v<typename Y::type>>;
};

template <class Y, uint8_t Scale, class Z>
struct mrm<Y, mul, disp8<Scale>, plus, Z> {
    template <typename T>
    using value = typename
      modrm<SIB<disp8<Scale>, extract_args_v<typename Y::type>, extract_args_v<typename Z::type>>, T>::value;

    static constexpr auto R = 0;
    static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
    static constexpr auto B = is_ext_v<extract_args_v<typename Z::type>>;

    using _67h = is_67h_needed<extract_head_v<typename Y::type>>;
};

template <class Y, uint8_t Scale, class Z, uint8_t D>
struct mrm<Y, mul, disp8<Scale>, plus, Z, plus, disp8<D>> {
    template <typename T>
    using value = typename
        modrm<disp_SIB<disp8<Scale>, disp8<D>, extract_args_v<typename Y::type>, extract_args_v<typename Z::type>>, T>::value;

    static constexpr auto R = 0;
    static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
    static constexpr auto B = is_ext_v<extract_args_v<typename Z::type>>;

    using _67h = is_67h_needed<extract_head_v<typename Y::type>>;
};

template <class Y, uint8_t Scale, class Z, uint8_t D>
struct mrm<Y, mul, disp8<Scale>, plus, Z, plus, disp32<D>> {
  template <typename T>
  using value = typename
      modrm<disp_SIB<disp8<Scale>, disp32<D>, extract_args_v<typename Y::type>,
                     extract_args_v<typename Z::type>>,
            T>::value;

  static constexpr auto R = 0;
  static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
  static constexpr auto B = is_ext_v<extract_args_v<typename Z::type>>;

  using _67h = is_67h_needed<extract_head_v<typename Y::type>>;
};

template<typename Y, typename ...T>
using mrm_v = typename mrm<T...>::template value<Y>;

//Utils
namespace {
    template <char first_letter, char... chars>
    struct first_elem {
      static constexpr auto value = first_letter;
    };

    template <char first_letter>
    struct first_elem<first_letter> {
      static constexpr auto value = first_letter;
    };

}
//Utils #2
namespace {

    template <uint64_t x, uint64_t y>
    struct _pow : std::integral_constant<uint64_t, _pow<x, y - 1>::value * x> {};

    template <uint64_t x>
    struct _pow<x, 1> : std::integral_constant<uint64_t, x> {};

    struct is_letter;
    struct is_digit;
    struct is_space;
    struct is_identifier_char;

    template <char C>
    struct is_same_as;

    template <char Symbol>
    using is_letter_v = std::bool_constant<((Symbol >= 'A') && (Symbol <= 'Z')) ||
                                           ((Symbol >= 'a') && (Symbol <= 'z'))>;

    template <char Symbol>
    using is_digit_v = std::bool_constant<(Symbol >= '0') && (Symbol <= '9')>;

    template <char Symbol>
    using is_space_v =
        std::bool_constant<Symbol == ' ' || Symbol == '\t' || Symbol == '\n'>;

    template <char Symbol>
    using is_identifier_char_v =
        std::bool_constant<is_letter_v<Symbol>::value ||
                           is_digit_v<Symbol>::value || Symbol == '_' || Symbol == '.'>;

    template <typename T, char C>
    struct find_predict;

    template<char C>
    struct find_predict<is_letter, C> {
        using type = is_letter_v<C>;
    };

    template <char C>
    struct find_predict<is_digit, C> {
      using type = is_digit_v<C>;
    };

    template <char C>
    struct find_predict<is_space, C> {
      using type = is_space_v<C>;
    };

    template <char C>
    struct find_predict<is_identifier_char, C> {
      using type = is_identifier_char_v<C>;
    };

    template <char M, char C>
    struct find_predict<is_same_as<M>, C> {
      using type = std::bool_constant<C == M>;
    };

    template<class T, char C>
    using find_predict_t = typename find_predict<T, C>::type;

    template<typename, class> struct skip_until{};

    template<char First, char... Str, class T>
    struct skip_until<char_seq<First, Str...>, T> {
        using type = std::conditional_t<find_predict_t<T, First>::value, char_seq<Str...>, typename skip_until<char_seq<Str...>, T>::type>;
    };

    template<class T>
    struct skip_until<char_seq<>, T>{
        using type = char_seq<>;
    };

    template <typename T, class Y>
    using skip_until_t = typename skip_until<T, Y>::type;

    template <typename, typename, class> struct collect_until {};

    template <char... Acc, char First, char... Str, class V>
    struct collect_until<char_seq<Acc...>, char_seq<First, Str...>, V> {
        using type = std::conditional_t<find_predict_t<V, First>::value, char_seq<Acc...>, typename collect_until<char_seq<Acc..., First>, char_seq<Str...>, V>::type>;
    };

    template <char... Acc, class V>
    struct collect_until<char_seq<Acc...>, char_seq<>, V> {
      using type = char_seq<Acc...>;
    };

    template <typename T, class V>
    using collect_until_t = typename collect_until<null_sequence<char>, T, V>::type;

    template <typename, std::size_t>
    struct skip_for {};

    template <char First, char... Str, std::size_t N>
    struct skip_for<char_seq<First, Str...>, N> {
        using type =
            std::conditional_t<N != 0,
                                typename skip_for<char_seq<Str...>, (N - 1)>::type,
                                char_seq<First, Str...>>;
    };

    template <std::size_t N>
    struct skip_for<char_seq<>, N> {
      using type = char_seq<>;
    };

    template <typename T, std::size_t N>
    using skip_for_t = typename skip_for<T, N>::type;

    template <typename, typename, class> struct collect_while {};

    template <char... Acc, char First, char... Str, class V>
    struct collect_while<char_seq<Acc...>, char_seq<First, Str...>, V> {
        using nV = std::conditional_t<find_predict_t<V, First>::value, V, void>;
        using nAcc = std::conditional_t<find_predict_t<V, First>::value, char_seq<Acc..., First>, char_seq<Acc...>>;
        using nNext = std::conditional_t<find_predict_t<V, First>::value, char_seq<Str...>, char_seq<First, Str...>>;
        using _local = collect_while<nAcc, nNext, nV>;
        using type = typename _local::type;
        using next = typename _local::next;
    };

    template <char... Acc, char First, char... Str>
    struct collect_while<char_seq<Acc...>, char_seq<First, Str...>, void> {
      using type = char_seq<Acc...>;
      using next = char_seq<First, Str...>;
    };

    template <char... Acc, class V>
    struct collect_while<char_seq<Acc...>, char_seq<>, V> {
      using type = char_seq<Acc...>;
       using next = char_seq<>;
    };

    template <typename T, class V>
    using collect_while_t = typename collect_while<null_sequence<char>, T, V>::type;

    template <typename T, class V>
    using skip_while_t = typename collect_while<null_sequence<char>, T, V>::next;

    namespace {
        template <typename T, typename Y, char Char>
        struct skip_c {};

        template <char... acc_str, char a, char... str, char Char>
        struct skip_c<char_seq<acc_str...>, char_seq<a, str...>, Char> {
          using value =
              typename skip_c<char_seq<acc_str..., a>, char_seq<str...>, Char>::value;
        };
        template <char... acc_str, char... str, char Char>
        struct skip_c<char_seq<acc_str...>, char_seq<Char, str...>, Char> {
          using value =
              typename skip_c<char_seq<acc_str...>, char_seq<str...>, Char>::value;
        };
        template <char... acc_str, char Char>
        struct skip_c<char_seq<acc_str...>, null_sequence<char>, Char> {
          using value = char_seq<acc_str...>;
        };

        template <typename T, typename Y>
        using skip_spaces = skip_c<T, Y, ' '>;
        template <typename T, typename Y>
        using skip_underscores = skip_c<T, Y, '_'>;

        template <typename T>
        using skip_spaces_t = typename skip_c<char_seq<>, T, ' '>::value;
        template <typename T>
        using skip_underscores_t = typename skip_c<char_seq<>, T, '_'>::value;
    }
}


namespace {
template <typename T, uint64_t N>
struct parse_num_impl : std::integral_constant<uint64_t, 0> {};

template <char first, char... str, uint64_t N>
struct parse_num_impl<char_seq<first, str...>, N> {
  static constexpr uint64_t value = parse_num_impl<char_seq<first>, N>::value *
                                        (_pow<N, sizeof...(str)>::value) +
                                    parse_num_impl<char_seq<str...>, N>::value;
};

template <char first, uint64_t N>
struct parse_num_impl<char_seq<first>, N> {
  static constexpr uint64_t value =
      ((first >= 'A') && (first <= 'F')) ? (first - 'A' + 10) : (first - '0');
};

template <typename T>
struct parse_num : parse_num_impl<T, 10> {};

template <char... str>
struct parse_num<char_seq<str...>> : parse_num_impl<char_seq<str...>, 10> {};

template <char... str>
struct parse_num<char_seq<'0', 'd', str...>>
    : parse_num_impl<char_seq<str...>, 10> {};

template <char... str>
struct parse_num<char_seq<'0', 'c', str...>>
    : parse_num_impl<char_seq<str...>, 16> {};

template <char... str>
struct parse_num<char_seq<'0', 'x', str...>>
    : parse_num_impl<char_seq<str...>, 16> {};

template <char... str>
struct parse_num<char_seq<'0', 'o', str...>>
    : parse_num_impl<char_seq<str...>, 8> {};

template <char... str>
struct parse_num<char_seq<'0', 'q', str...>>
    : parse_num_impl<char_seq<str...>, 8> {};

template <char... str>
struct parse_num<char_seq<'0', 'b', str...>>
    : parse_num_impl<char_seq<str...>, 2> {};

template <char... str>
struct parse_num<char_seq<'0', 'y', str...>>
    : parse_num_impl<char_seq<str...>, 2> {};

template <char... str>
struct parse_num<char_seq<'0', 'x', 'c', str...>>
    : parse_num_impl<char_seq<str...>, 16> {};

template <char... str>
struct parse_num<char_seq<'0', 'h', 'c', str...>>
    : parse_num_impl<char_seq<str...>, 16> {};
}

namespace {
    template <typename T> struct typeDef { using type = T; };
    template <char... S> struct _reg_impl: typeDef<void> {};

    template <> struct _reg_impl<'e', 's'> : typeDef<es> {};
    template <> struct _reg_impl<'c', 's'> : typeDef<cs> {};
    template <> struct _reg_impl<'s', 's'> : typeDef<ss> {};
    template <> struct _reg_impl<'d', 's'> : typeDef<ds> {};
    template <> struct _reg_impl<'f', 's'> : typeDef<fs> {};
    template <> struct _reg_impl<'g', 's'> : typeDef<gs> {};
    
    template <> struct _reg_impl<'a', 'l'> : typeDef<al> {};
    template <> struct _reg_impl<'c', 'l'> : typeDef<cl> {};
    template <> struct _reg_impl<'d', 'l'> : typeDef<dl> {};
    template <> struct _reg_impl<'b', 'l'> : typeDef<bl> {};
    template <> struct _reg_impl<'a', 'h'> : typeDef<ah> {};
    template <> struct _reg_impl<'c', 'h'> : typeDef<ch> {};
    template <> struct _reg_impl<'d', 'h'> : typeDef<dh> {};
    template <> struct _reg_impl<'b', 'h'> : typeDef<bh> {};
          
    template <> struct _reg_impl<'r', '8', 'l'>      : typeDef<r8l> {};
    template <> struct _reg_impl<'r', '9', 'l'>      : typeDef<r9l> {};
    template <> struct _reg_impl<'r', '1', '0', 'l'> : typeDef<r10l> {};
    template <> struct _reg_impl<'r', '1', '1', 'l'> : typeDef<r11l> {};
    template <> struct _reg_impl<'r', '1', '2', 'l'> : typeDef<r12l> {};
    template <> struct _reg_impl<'r', '1', '3', 'l'> : typeDef<r13l> {};
    template <> struct _reg_impl<'r', '1', '4', 'l'> : typeDef<r14l> {};
    template <> struct _reg_impl<'r', '1', '5', 'l'> : typeDef<r15l> {};

    template <> struct _reg_impl<'a', 'x'> : typeDef<ax> {};
    template <> struct _reg_impl<'b', 'x'> : typeDef<bx> {};
    template <> struct _reg_impl<'c', 'x'> : typeDef<cx> {};
    template <> struct _reg_impl<'d', 'x'> : typeDef<dx> {};
    template <> struct _reg_impl<'s', 'i'> : typeDef<si> {};
    template <> struct _reg_impl<'d', 'i'> : typeDef<di> {};
    template <> struct _reg_impl<'s', 'p'> : typeDef<sp> {};
    template <> struct _reg_impl<'b', 'p'> : typeDef<bp> {};

    template <> struct _reg_impl<'r', '8', 'w'>      : typeDef<r8w> {};
    template <> struct _reg_impl<'r', '9', 'w'>      : typeDef<r9w> {};
    template <> struct _reg_impl<'r', '1', '0', 'w'> : typeDef<r10w> {};
    template <> struct _reg_impl<'r', '1', '1', 'w'> : typeDef<r11w> {};
    template <> struct _reg_impl<'r', '1', '2', 'w'> : typeDef<r12w> {};
    template <> struct _reg_impl<'r', '1', '3', 'w'> : typeDef<r13w> {};
    template <> struct _reg_impl<'r', '1', '4', 'w'> : typeDef<r14w> {};
    template <> struct _reg_impl<'r', '1', '5', 'w'> : typeDef<r15w> {};

    template <> struct _reg_impl<'e', 'a', 'x'> : typeDef<eax> {};
    template <> struct _reg_impl<'e', 'b', 'x'> : typeDef<ebx> {};
    template <> struct _reg_impl<'e', 'c', 'x'> : typeDef<ecx> {};
    template <> struct _reg_impl<'e', 'd', 'x'> : typeDef<edx> {};
    template <> struct _reg_impl<'e', 's', 'i'> : typeDef<esi> {};
    template <> struct _reg_impl<'e', 'd', 'i'> : typeDef<edi> {};
    template <> struct _reg_impl<'e', 's', 'p'> : typeDef<esp> {};
    template <> struct _reg_impl<'e', 'b', 'p'> : typeDef<ebp> {};

    template <> struct _reg_impl<'z', 'a', 'x'> : typeDef<zax> {};
    template <> struct _reg_impl<'z', 'b', 'x'> : typeDef<zbx> {};
    template <> struct _reg_impl<'z', 'c', 'x'> : typeDef<zcx> {};
    template <> struct _reg_impl<'z', 'd', 'x'> : typeDef<zdx> {};
    template <> struct _reg_impl<'z', 's', 'i'> : typeDef<zsi> {};
    template <> struct _reg_impl<'z', 'd', 'i'> : typeDef<zdi> {};
    template <> struct _reg_impl<'z', 's', 'p'> : typeDef<zsp> {};
    template <> struct _reg_impl<'z', 'b', 'p'> : typeDef<zbp> {};

    template <> struct _reg_impl<'r', '8', 'd'>      : typeDef<r8d> {};
    template <> struct _reg_impl<'r', '9', 'd'>      : typeDef<r9d> {};
    template <> struct _reg_impl<'r', '1', '0', 'd'> : typeDef<r10d> {};
    template <> struct _reg_impl<'r', '1', '1', 'd'> : typeDef<r11d> {};
    template <> struct _reg_impl<'r', '1', '2', 'd'> : typeDef<r12d> {};
    template <> struct _reg_impl<'r', '1', '3', 'd'> : typeDef<r13d> {};
    template <> struct _reg_impl<'r', '1', '4', 'd'> : typeDef<r14d> {};
    template <> struct _reg_impl<'r', '1', '5', 'd'> : typeDef<r15d> {};

    template <> struct _reg_impl<'r', 'a', 'x'> : typeDef<rax> {};
    template <> struct _reg_impl<'r', 'b', 'x'> : typeDef<rbx> {};
    template <> struct _reg_impl<'r', 'c', 'x'> : typeDef<rcx> {};
    template <> struct _reg_impl<'r', 'd', 'x'> : typeDef<rdx> {};
    template <> struct _reg_impl<'r', 's', 'i'> : typeDef<rsi> {};
    template <> struct _reg_impl<'r', 'd', 'i'> : typeDef<rdi> {};
    template <> struct _reg_impl<'r', 's', 'p'> : typeDef<rsp> {};
    template <> struct _reg_impl<'r', 'b', 'p'> : typeDef<rbp> {};
    
    template <> struct _reg_impl<'r', 'i', 'p'> : typeDef<rip> {};

    template <> struct _reg_impl<'r', '8'>      : typeDef<r8> {};
    template <> struct _reg_impl<'r', '9'>      : typeDef<r9> {};
    template <> struct _reg_impl<'r', '1', '0'> : typeDef<r10> {};
    template <> struct _reg_impl<'r', '1', '1'> : typeDef<r11> {};
    template <> struct _reg_impl<'r', '1', '2'> : typeDef<r12> {};
    template <> struct _reg_impl<'r', '1', '3'> : typeDef<r13> {};
    template <> struct _reg_impl<'r', '1', '4'> : typeDef<r14> {};
    template <> struct _reg_impl<'r', '1', '5'> : typeDef<r15> {};

    template <char... chars> struct _reg_impl<'s', 't', chars...> {
    using type = st<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
    };
    template <char... chars> struct _reg_impl<'m', 'm', chars...> {
    using type = mm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
    };
    template <char... chars> struct _reg_impl<'x', 'm', 'm', chars...> {
    using type = xmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
    };
    template <char... chars> struct _reg_impl<'y', 'm', 'm', chars...> {
    using type = ymm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
    };
    template <char... chars> struct _reg_impl<'z', 'm', 'm', chars...> {
    using type = zmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
    };
    template <char... chars> struct _reg_impl<'t', 'm', 'm', chars...> {
    using type = tmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
    };

    template <typename T>
    struct get_reg;

    template<char... S>
    struct get_reg<char_seq<S...>> : _reg_impl<S...> {};

    template<typename T>
    using get_reg_t = typename get_reg<T>::type;

    template<std::size_t N, typename T>
    using _pack_value_impl = std::bool_constant<(std::numeric_limits<T>::min() <= N) && (N <= std::numeric_limits<T>::max())>;
    template<std::size_t N>
    using pack_value =  std::conditional_t<_pack_value_impl<N, uint8_t >::value, ub<static_cast<uint8_t >(N)>,
                        std::conditional_t<_pack_value_impl<N, uint16_t>::value, uw<static_cast<uint16_t>(N)>, 
                        std::conditional_t<_pack_value_impl<N, uint32_t>::value, ud<static_cast<uint32_t>(N)>,
                        std::conditional_t<_pack_value_impl<N, uint64_t>::value, uq<static_cast<uint64_t>(N)>, 
                        void>>>>;
}

namespace {
    template<char... Str>
    struct _word_size_impl : typeDef<void>{};

    template<> struct _word_size_impl<'b', 'y', 't', 'e'> : typeDef<reg8> {};
    template<> struct _word_size_impl<'w', 'o', 'r', 'd'> : typeDef<reg16> {};
    template<> struct _word_size_impl<'d', 'w', 'o', 'r', 'd'> : typeDef<reg32> {};
    template<> struct _word_size_impl<'q', 'w', 'o', 'r', 'd'> : typeDef<reg64> {};
    template<> struct _word_size_impl<'t', 'w', 'o', 'r', 'd'> : typeDef<reg80> {};
    template<> struct _word_size_impl<'z', 'w', 'o', 'r', 'd'> : typeDef<reg128> {};

    template<typename T>
    struct get_word_sz : typeDef<void> {};

    template<char... Str>
    struct get_word_sz<char_seq<Str...>> : _word_size_impl<Str...>{};

    template<typename T>
    using get_word_sz_t = typename get_word_sz<T>::type;
}
//lexer
namespace {

    enum Tokens: uint8_t{
      BaseId = 0,
      Identifier,
      SizeWord,
      Number,
      LeftParen,
      RightParen,
      LeftSquare,
      RightSquare,
      Ptr,
      Plus,
      Minus,
      Mul,
      Div,
      DoubleDiv,
      Modulo,
      DoubleModulo,
      Or,
      DoubleOr,
      And,
      DoubleAnd,
      Xor,
      DoubleXor,
      Dollar,
      DoubleDollar,
      Dup,
      Dot,
      Colon,
      Comma,
      End
    };
    
    template <Tokens Id, typename Y>
    struct Token {
        static constexpr Tokens id = Id;
        using data = Y;
    };

    
    template <Tokens>  static constexpr uint8_t TokenPrec =  0;

    template <> constexpr uint8_t TokenPrec<Tokens::Plus> = 1;
    template <> constexpr uint8_t TokenPrec<Tokens::Minus> =  1;
    template <> constexpr uint8_t TokenPrec<Tokens::Mul> =  2;
    template <> constexpr uint8_t TokenPrec<Tokens::Div> =  2;
    template <> constexpr uint8_t TokenPrec<Tokens::Modulo> = 3;
    template <> constexpr uint8_t TokenPrec<Tokens::DoubleOr> = 4;
    template <> constexpr uint8_t TokenPrec<Tokens::Or> = 5;
    template <> constexpr uint8_t TokenPrec<Tokens::And> = 6;
    template <> constexpr uint8_t TokenPrec<Tokens::DoubleAnd> = 9;
    template <> constexpr uint8_t TokenPrec<Tokens::Xor> = 10;
    template <> constexpr uint8_t TokenPrec<Tokens::DoubleXor> = 11;
    template <> constexpr uint8_t TokenPrec<Tokens::Dup> =  12;
    template <> constexpr uint8_t TokenPrec<Tokens::Dot> = 13;

    template <Tokens> static constexpr std::string_view TokenName = "None";

    template <> constexpr std::string_view TokenName<Tokens::BaseId> = "BaseId"; 
    template <> constexpr std::string_view TokenName<Tokens::Identifier> = "Identifier"; 
    template <> constexpr std::string_view TokenName<Tokens::SizeWord> = "SizeWord"; 
    template <> constexpr std::string_view TokenName<Tokens::Number> = "Number"; 
    template <> constexpr std::string_view TokenName<Tokens::LeftParen> = "LeftParen"; 
    template <> constexpr std::string_view TokenName<Tokens::RightParen> = "RightParen"; 
    template <> constexpr std::string_view TokenName<Tokens::LeftSquare> = "LeftSquare"; 
    template <> constexpr std::string_view TokenName<Tokens::RightSquare> = "RightSquare"; 
    template <> constexpr std::string_view TokenName<Tokens::Ptr> = "Ptr"; 
    template <> constexpr std::string_view TokenName<Tokens::Plus> = "Plus"; 
    template <> constexpr std::string_view TokenName<Tokens::Minus> = "Minus"; 
    template <> constexpr std::string_view TokenName<Tokens::Mul> = "Mul"; 
    template <> constexpr std::string_view TokenName<Tokens::Div> = "Div"; 
    template <> constexpr std::string_view TokenName<Tokens::Or> = "Or";
    template <> constexpr std::string_view TokenName<Tokens::DoubleOr> = "DoubleOr";
    template <> constexpr std::string_view TokenName<Tokens::And> = "And"; 
    template <> constexpr std::string_view TokenName<Tokens::DoubleAnd> = "DoubleAnd";
    template <> constexpr std::string_view TokenName<Tokens::Xor> = "Xor"; 
    template <> constexpr std::string_view TokenName<Tokens::DoubleXor> = "DoubleXor"; 
    template <> constexpr std::string_view TokenName<Tokens::Modulo> = "Modulo"; 
    template <> constexpr std::string_view TokenName<Tokens::Dollar> = "Dollar";
    template <> constexpr std::string_view TokenName<Tokens::DoubleDollar> = "DoubleDollar";
    template <> constexpr std::string_view TokenName<Tokens::Dup> = "Dup";
    template <> constexpr std::string_view TokenName<Tokens::Dot> = "Dot"; 
    template <> constexpr std::string_view TokenName<Tokens::Colon> = "Colon"; 
    template <> constexpr std::string_view TokenName<Tokens::Comma> = "Comma"; 
    template <> constexpr std::string_view TokenName<Tokens::End> = "End"; 

    template <std::size_t N, std::size_t K, typename... T>
    struct lex {};

    template <std::size_t N, char... Str>
    struct lex<N, 0, char_seq<Str...>> {
      using next = char_seq<>;

      using type = hold<>;
      using nN = std::integral_constant<std::size_t, N>;
    };

    template <std::size_t N, std::size_t Max, char... Str>
    struct lex<N, Max, char_seq<Str...>> {

      using _first_part = lex<N + 1, (Max / 2), char_seq<Str...>>;

      using _next = typename _first_part::next;

      using _second_part = lex<N + 1, (Max / 2) + (_next{}.size() % 2), _next>;

      using nn0 = typename _first_part::nN;
      using nn1 = typename _second_part::nN;
      using nN = std::integral_constant<
          std::size_t, (nn0::value > nn1::value ? nn0::value : nn1::value)>;
      using next = typename _second_part::next;

      using type = type_list_append_t<typename _first_part::type,
                                      typename _second_part::type>;
    };

    template <std::size_t N, char FLetter, char... Str>
    struct lex<N, 1, char_seq<FLetter, Str...>> {
      using if_digit = std::conditional_t<
          is_digit_v<FLetter>::value,
              hold<Token<
                       Tokens::Number,
                       std::integral_constant<std::size_t, parse_num<collect_while_t<char_seq<FLetter, Str...>, is_digit>>::value>>>,
          void>;
      using if_word =
          std::conditional_t<
          std::is_same<if_digit, void>::value &&
              ((FLetter >= 'A' && FLetter <= 'Z') ||
               (FLetter >= 'a' && FLetter <= 'z') || FLetter == '.'),
                  hold<Token<Tokens::Identifier, collect_while_t<char_seq<FLetter, Str...>, is_identifier_char>>>, void>;
        using if_smth = std::conditional<!std::is_same<if_digit, void>::value, if_digit, std::conditional_t<!std::is_same<if_word, void>::value, if_word, hold<>>>;
        using type = typename if_smth::type;
        using next =
            std::conditional_t<
                !std::is_same<if_digit, void>::value,
                skip_while_t<char_seq<FLetter, Str...>, is_digit>,
                std::conditional_t<
                    !std::is_same<if_word, void>::value,
                    skip_while_t<char_seq<Str...>, is_identifier_char>,
                char_seq<Str...>> > ;

        using nN = std::integral_constant<std::size_t, N>;
    };

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<' ', Str...>> {
      using type = hold<>;
      using next = char_seq<Str...>;

      using nN = std::integral_constant<std::size_t, N>;
    };

    template <std::size_t N>
    struct lex<N, 1, char_seq<>> {
      using type = hold<>;
      using next = char_seq<>;

      using nN = std::integral_constant<std::size_t, N>;
    };

    template <std::size_t N, Tokens Tok, char ...Str> 
    struct tok_templ {
      using type = hold<Token<Tok, void>>;
      using next = char_seq<Str...>;

      using nN = std::integral_constant<std::size_t, N>;
    };

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<')', Str...>>
        : tok_templ<N, Tokens::RightParen, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'(', Str...>>
        : tok_templ<N, Tokens::LeftParen, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'[', Str...>>
        : tok_templ<N, Tokens::LeftSquare, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<']', Str...>>
        : tok_templ<N, Tokens::RightSquare, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'+', Str...>>
        : tok_templ<N, Tokens::Plus, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'-', Str...>>
        : tok_templ<N, Tokens::Minus, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'*', Str...>>
        : tok_templ<N, Tokens::Mul, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'/', Str...>>
        : tok_templ<N, Tokens::Div, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'%', Str...>>
        : tok_templ<N, Tokens::Modulo, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'$', Str...>>
        : tok_templ<N, Tokens::Dollar, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'$', '$', Str...>>
        : tok_templ<N, Tokens::DoubleDollar, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'|', Str...>>
        : tok_templ<N, Tokens::Or, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'|', '|', Str...>>
        : tok_templ<N, Tokens::DoubleOr, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'&', Str...>>
        : tok_templ<N, Tokens::And, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'&', '&', Str...>>
        : tok_templ<N, Tokens::DoubleAnd, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'^', Str...>>
        : tok_templ<N, Tokens::Xor, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'^', '^', Str...>>
        : tok_templ<N, Tokens::DoubleXor, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'d', 'u', 'p', Str...>>
        : tok_templ<N, Tokens::Dup, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<',', Str...>>
        : tok_templ<N, Tokens::Comma, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<':', Str...>>
        : tok_templ<N, Tokens::Colon, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<';', Str...>>
        : tok_templ<N, Tokens::End, Str...> {};

    template <std::size_t N, char... Str>
    struct lex<N, 1, char_seq<'\x00', Str...>>
        : tok_templ<N, Tokens::End, Str...> {};
}

//ast parse
namespace {
    template <Tokens Opc, class L, class R>
    struct node {
        static constexpr Tokens id = Opc;
        using left = L;
        using right = R;
    };

    template <class D>
    struct primary_node {
        static constexpr Tokens id = Tokens::BaseId;
        using left = D;
    };

    template <Tokens Opc, class L>
    struct unary_node {
        static constexpr Tokens id = Opc;
        using left = L;
    };

    template<class Name, class Ops>
    struct instr_node {
        using name = Name;
        using operands = Ops;
    };

    template<class Name, class Ops>
    struct label_node {
        using name = Name;
        using data = Ops;
    };

    template <class Names>
    struct labelref_node {
        using name = Names;
    };

    template <typename... T>
    struct parse_expr;

    template <typename... T>
    struct parse_paren;

    template <typename... T>
    struct parse_ptr;

    template <typename... T>
    struct parse_labelref;

    template <class ...Toks>
    struct parse_paren<hold<Toks...>> {
        using expr = parse_expr<hold<Toks...>>;
        using type = typename expr::type;
        using next = typename expr::next;
    };

    template <class... Toks>
    struct parse_ptr<hold<Toks...>> {
      using expr = parse_expr<hold<Toks...>>;
      using type = unary_node<Tokens::Ptr, typename expr::type>;
      using next = typename expr::next;
    };

    template <class Data, class... Toks>
    struct parse_labelref<hold<Token<Tokens::Identifier, Data>, Toks...>> {
      using type = labelref_node<Token<Tokens::Identifier, Data>>;
      using next = hold<Toks...>;
    };

    template<typename ...T>
    struct parse_primary;

    template<class ...Toks, class data>
    struct parse_primary<hold<Token<Tokens::Identifier, data>, Toks...>>{
        using type = primary_node<Token<Tokens::Identifier, data>>;
        using next = hold<Toks...>;
    };

    template<class ...Toks, class data>
    struct parse_primary<hold<Token<Tokens::Number, data>, Toks...>>{
        using type = primary_node<Token<Tokens::Number, data>>;
        using next = hold<Toks...>;
    };

    template<class ...Toks, class data>
    struct parse_primary<hold<Token<Tokens::LeftParen, data>, Toks...>>{
        using expr = parse_paren<hold<Toks...>>;
        using type = typename expr::type;
        using next = typename expr::next;
    };

    template<class ...Toks, class data>
    struct parse_primary<hold<Token<Tokens::LeftSquare, data>, Toks...>>{
        using expr = parse_paren<hold<Toks...>>;
        using type = typename expr::type;
        using next = typename expr::next;
    };

    template<class ...Toks, class data>
    struct parse_primary<hold<Token<Tokens::Ptr, data>, Toks...>> {
        using expr = parse_ptr<hold<Toks...>>;
        using type = typename expr::type;
        using next = typename expr::next;
    };

    template <class... Toks>
    struct parse_size_word;

    template <class... Toks, class Data>
    struct parse_size_word<
        hold<Token<Tokens::SizeWord, Data>, Toks...>> {
      using expr = parse_expr<hold<Toks...>>;
      using type = unary_node<Tokens::SizeWord, hold<get_word_sz_t<Data>, typename expr::type>>;
      using next = typename expr::next;
    };

    template<typename ...T>
    struct parse_unary;

    template<class ...Toks, class data>
    struct parse_unary<hold<Token<Tokens::Identifier, data>, Toks...>> {
        using expr = std::conditional_t<std::is_same<get_reg_t<data>, void>::value,
            std::conditional_t<!std::is_same<get_word_sz_t<data>, void>::value,
                parse_size_word<hold<Token<Tokens::SizeWord, data>, Toks...>>,
                parse_labelref<hold<Token<Tokens::Identifier, data>, Toks...>>>,
            parse_primary<hold<Token<Tokens::Identifier, data>, Toks...>>
        >;
        using type = typename expr::type;
        using next = typename expr::next;
    };
    template<class ...Toks, class data>
    struct parse_unary<hold<Token<Tokens::Number, data>, Toks...>> {
        using expr = parse_primary<hold<Token<Tokens::Number, data>, Toks...>>;
        using type = typename expr::type;
        using next = typename expr::next;
    };

    template<class ...Toks, class data>
    struct parse_unary<hold<Token<Tokens::LeftParen, data>, Toks...>> {
        using expr = parse_primary<hold<Token<Tokens::LeftParen, data>, Toks...>>;
        using type = typename expr::type;
        using next = typename expr::next;
    };

    template<class ...Toks, class data>
    struct parse_unary<hold<Token<Tokens::LeftSquare, data>, Toks...>> {
        using expr = parse_primary<hold<Token<Tokens::Ptr, data>, Toks...>>;
        using type = typename expr::type;
        using next = typename expr::next;
    };

    template <class... Toks>
    struct parse_unary<
        hold<Token<Tokens::Identifier, char_seq<'p', 't', 'r'>>,
        Token<Tokens::LeftSquare, void>,
        Toks...>> {
        using expr = parse_primary<hold<Token<Tokens::Ptr, void>, Toks...>>;
        using type = typename expr::type;
        using next = typename expr::next;
    };

    template <class... Toks>
    struct parse_unary<hold<Token<Tokens::Dollar, void>, Toks...>> {
      using type = primary_node<Token<Tokens::Dollar, void>>;
      using next = hold<Toks...>;
    };

    template <class... Toks>
    struct parse_unary<
        hold<Token<Tokens::DoubleDollar, void>, Toks...>> {
        using type = primary_node<Token<Tokens::DoubleDollar, void>>;
        using next = hold<Toks...>;
    };

    template<Tokens Id, class ...Toks, class data>
    struct parse_unary<hold<Token<Id, data>, Toks...>> {
        using expr = parse_unary<hold<Toks...>>;
        using type = unary_node<Id, typename expr::type>;
        using next = typename expr::next;
    };  

    template<>
    struct parse_unary<hold<>>{
      using type = hold<>;
      using next = hold<>;
    };

    template<std::size_t ExprPrec, typename... T>
    struct parse_binOp;

    template <std::size_t ExprPrec, typename... T>
    struct parse_binOp_;

    template <std::size_t ExprPrec, class LHS, Tokens Id, class data,
              class... Toks>
    struct parse_binOp<ExprPrec, LHS,
                       hold<Token<Id, data>, Toks...>> {
        using is_expr_higher = std::bool_constant<(ExprPrec > TokenPrec<Id>)>;
        using RHS = std::conditional_t<is_expr_higher::value, LHS, parse_unary<hold<Toks...>>>;
        
        using is_next_higher = std::bool_constant<(TokenPrec<RHS::next::head::id> > TokenPrec<Id>)>;
        using nRHS = std::conditional_t<is_next_higher::value, 
                parse_binOp<TokenPrec<Id>, typename RHS::type, typename RHS::next>, 
                RHS>;
        
        using next0 = typename nRHS::next;
        using type = std::conditional_t<is_expr_higher::value, LHS, typename parse_binOp<ExprPrec, node<Id, LHS, typename nRHS::type>, next0>::type>;
        using next = std::conditional_t<is_expr_higher::value, hold<Token<Id, data>, Toks...>, typename parse_binOp<ExprPrec, node<Id, LHS, typename nRHS::next>, next0>::next>;

    };

    template <std::size_t ExprPrec, class LHS>
    struct parse_binOp<ExprPrec, LHS, hold<>> {
        using type = LHS;
        using next = hold<>;
    };

    template <std::size_t ExprPrec, class LHS, class data, class... Toks>
    struct parse_binOp<ExprPrec, LHS, hold<Token<Tokens::End, data>, Toks...>> {
      using type = LHS;
      using next = hold<>;
    };

    template <std::size_t ExprPrec, class LHS, class data, class ...Toks>
    struct parse_binOp<ExprPrec, LHS, hold<Token<Tokens::RightParen, data>, Toks...>> {
      using type = LHS;
      using next = std::conditional_t<ExprPrec < 1, hold<Toks...>, hold<Token<Tokens::RightParen, data>, Toks...>>;
    };

    template <std::size_t ExprPrec, class LHS, class data, class ...Toks>
    struct parse_binOp<ExprPrec, LHS, hold<Token<Tokens::RightSquare, data>, Toks...>> {
      using type = LHS;
      using next = std::conditional_t<ExprPrec < 1, hold<Toks...>, hold<Token<Tokens::RightParen, data>, Toks...>>;
    };

    template <std::size_t ExprPrec, class LHS, class data, class ...Toks>
    struct parse_binOp<ExprPrec, LHS, hold<Token<Tokens::Comma, data>, Toks...>> {
      using type = LHS;
      using next = hold<Toks...>;
    };
    
    template<class ...Toks> 
    struct parse_expr<hold<Toks...>> {
        using LHS = parse_unary<hold<Toks...>>;

        using Expr = parse_binOp<0, typename LHS::type, typename LHS::next>;
        using type = typename Expr::type;

        using next = typename Expr::next;
    };
     
    template <typename... T>
    struct parse_operands;

    template<class ...Acc, class Tok, class ...Toks>
    struct parse_operands<hold<Acc...>, hold<Tok, Toks...>>{
        using _next_ops = parse_operands<hold<Acc..., Tok>, hold<Toks...>>;
        using type = typename _next_ops::type;
        using next = typename _next_ops::next;
    };
    template <class... Acc, class data, class... Toks>
    struct parse_operands<hold<Acc...>, hold<Token<Tokens::Comma, data>, Toks...>> {
        using _next_ops = parse_operands<hold<>, hold<Toks...>>;
        using type = type_list_append_t<hold<typename parse_expr<hold<Acc..., Token<Tokens::End, char_seq<>>>>::type>, typename _next_ops::type>;
        using next = typename _next_ops::next;
    };

    template <class... Acc, class data, class... Toks>
    struct parse_operands<hold<Acc...>, hold<Token<Tokens::End, data>, Toks...>> {
        using type = hold<typename parse_expr<hold<Acc..., Token<Tokens::End, data>>>::type>;
        using next = hold<Toks...>;
    };

    template <typename... T>
    struct parse_instruction;

    template<class ...Toks>
    struct parse_instruction<hold<Toks...>> {
        using _name = parse_primary<hold<Toks...>>;
        using name = typename _name::type;

        using next_0 = typename _name::next;

        using _operand = parse_operands<hold<>, next_0>;
        using operands = typename _operand::type;

        using type = instr_node<name, operands>;
        using next = typename _operand::next;
    };

    template<typename T>
    struct prefix_name_sz;

    template <char... Name>
    struct prefix_name_sz<char_seq<Name...>> {
      static constexpr std::size_t value = 1;
    };

    template<char ...Name>
    struct prefix_name_sz<char_seq<'.', Name...>> {
      static constexpr std::size_t value = 1 + prefix_name_sz<char_seq<Name...>>::value;
    };

    template <std::size_t, typename... T>
    struct parse_label_or_data;
    
    template <char... Name, class... Toks, class data>
    struct parse_label_or_data<
        0, hold<Token<Tokens::Identifier, char_seq<Name...>>,
                Token<Tokens::Colon, data>, Toks...>> {
        using name = primary_node<Token<Tokens::Identifier,
                                    char_seq<'g', 'l', 'o', 'b', 'a', 'l'>>>;
        using _local = parse_label_or_data<
          1, hold<Token<Tokens::Identifier, char_seq<Name...>>,
                  Token<Tokens::Colon, data>, Toks...>>;

        using type = hold<label_node<name, typename _local::type>>; 
    };

    template <class... Toks>
    struct parse_label_or_data<0, hold<Toks...>> {
        using name = primary_node<Token<Tokens::Identifier,
                                    char_seq<'g', 'l', 'o', 'b', 'a', 'l'>>>;
        using _local = parse_instruction<hold<Toks...>>;
        using _next = typename _local::next;

        using __local = parse_label_or_data<1, _next>;
        using type = hold<label_node<name,type_list_append_t<hold<typename _local::type>, typename __local::type>>>;
        using next = typename __local::next;
    };

    template <std::size_t Prefix, char... Name, class... Toks, class data>
    struct parse_label_or_data<
        Prefix,
        hold<Token<Tokens::Identifier, char_seq< Name...>>,
             Token<Tokens::Colon, data>, Toks...>> {

        using name = primary_node<Token<Tokens::Identifier, char_seq<Name...>>>;

        using is_exact_prefix = std::bool_constant<Prefix <= prefix_name_sz<char_seq<Name...>>::value>;

        using _local = parse_label_or_data<Prefix + 1, hold<Toks...>>;
        using _next = typename _local::next;

        using __local = std::conditional_t<is_exact_prefix::value, parse_label_or_data<Prefix, _next>, _local>;
        using _type =  hold<label_node<name, typename _local::type>>;

        using type = std::conditional_t<is_exact_prefix::value, type_list_append_t<_type, typename __local::type>, hold<>>;
        using next =
            std::conditional_t<
                is_exact_prefix::value, typename __local::next,
                hold<Token<Tokens::Identifier, char_seq<Name...>>,
                     Token<Tokens::Colon, data>, Toks...>>;
    };

    template <std::size_t, typename... T>
    struct parse_instructions;

    template <std::size_t N>
    struct parse_instructions<N, hold<>> {
      using next = hold<>;
      using type = hold<>;
    };

    template <>
    struct parse_instructions<1, hold<>>{
      using next = hold<>;
      using type = hold<>;
    };

    template <>
    struct parse_instructions<1, hold<Token<Tokens::End, void>>>{
      using next = hold<>;
      using type = hold<>;
    };

    template <std::size_t N, class Data, class... Toks>
    struct parse_instructions<N, hold<Token<Tokens::Identifier, Data>,
                                      Token<Tokens::Colon, void>, Toks...>> {
        using next = hold<Token<Tokens::Identifier, Data>,
                          Token<Tokens::Colon, void>, Toks...>;
        using type = hold<>;
    };

    template <class Data, class... Toks>
    struct parse_instructions<1, hold<Token<Tokens::Identifier, Data>,
                                      Token<Tokens::Colon, void>, Toks...>> {
      using next = hold<Token<Tokens::Identifier, Data>,
                        Token<Tokens::Colon, void>, Toks...>;
      using type = hold<>;
    };

    template <std::size_t N, class... Toks>
    struct parse_instructions<N, hold<Toks...>> {
        using _first_part = parse_instructions<N / 2, hold<Toks...>>;
        using _second_part = parse_instructions<N / 2, typename _first_part::next>;
        using _next = typename _second_part::next;

        using _fin_part = parse_instructions<N*4, _next>;

        using next = typename _fin_part::next;
        using type = type_list_append_t<typename _first_part::type, typename _second_part::type, typename _fin_part::type>;
    };

    template <class... Toks>
    struct parse_instructions<1, hold<Toks...>> {
      using _local = parse_instruction<hold<Toks...>>;
      using next = typename _local::next;
      using type = hold<typename _local::type>;
    };

    template <class... Toks>
    struct parse_instructions<0,  hold<Toks...>> {
      using _local = parse_instruction<hold<Toks...>>;
      using next = typename _local::next;
      using type = hold<typename _local::type>;
    };

    template <std::size_t Prefix, class... Toks>
    struct parse_label_or_data<Prefix, hold<Toks...>> {
        using _local = parse_instructions<4, hold<Toks...>>;
        using _next = typename _local::next;

        using __local = parse_label_or_data<Prefix, _next>;
        using type = type_list_append_t<typename _local::type, typename __local::type>;
        using next = typename __local::next;
    };

    template <std::size_t Prefix>
    struct parse_label_or_data<Prefix, hold<>> {
        using next = hold<>;
        using type = hold<>;
    };

    template <std::size_t Prefix, class Data, class... Toks>
    struct parse_label_or_data<Prefix, hold<Token<Tokens::End, Data>, Toks...>> {
        using next = hold<Toks...>;
        using type = hold<>;
    };

    template<class Toks>
    using parse_global = parse_label_or_data<0, Toks>;
}

// AST solver
namespace {
    template<typename ...T>
    struct ast_solve;

    template<>
    struct ast_solve<hold<>> {
        using type = hold<>;
    };

    template<class Left>
    struct ast_solve<primary_node<Left>> {
        using type = primary_node<Left>;
    };

    template<class... Data>
    struct ast_solve<hold<Data...>> {
        using type = type_list_append_t<
            hold<typename ast_solve<typename hold<Data...>::head>::type>,
            typename ast_solve<typename hold<Data...>::tail>::type>;
    };

    template<class Name, class Data>
    struct ast_solve<label_node<Name, Data>> {
        using type = label_node<Name, typename ast_solve<Data>::type>;
    };

    template<class Name, class Ops>
    struct ast_solve<instr_node<Name, Ops>> {
        using type = instr_node<Name, typename ast_solve<Ops>::type>;
    };

    template <Tokens Id, class Op>
    struct ast_solve<unary_node<Id, Op>> {
        using type = unary_node<Id, typename ast_solve<Op>::type>;
    };

    template<class Op>
    struct ast_solve<unary_node<Tokens::LeftParen, Op>> {
        using type = typename ast_solve<Op>::type;
    };

    template <class Data>
    struct ast_solve<primary_node<Token<Tokens::Identifier, Data>>> {
        using type = primary_node<Token<Tokens::Identifier, Data>>;
    };

    template<class Data>
    struct ast_solve<primary_node<Token<Tokens::Number, Data>>> {
        using type = primary_node<Token<Tokens::Number, Data>>;
    };

    template<class RegSize, class Op>
    struct ast_solve<unary_node<Tokens::SizeWord, hold<RegSize, Op>>> {
        using type = unary_node<Tokens::SizeWord, hold<RegSize, typename ast_solve<Op>::type>>;
    };

    template<class Op>
    struct ast_solve<unary_node<Tokens::Ptr, Op>> {
        using type = unary_node<Tokens::Ptr,  typename ast_solve<Op>::type>;
    };

    template <Tokens Id, class Left, class Right>
    struct ast_solve<node<Id, Left, Right>> {
        using type = node<Id, typename ast_solve<Left>::type, typename ast_solve<Right>::type>;
    };

    template <Tokens Id, class LData, class Right>
    struct ast_solve<
        node<Id, primary_node<Token<Tokens::Identifier, LData>>, Right>> {
        using type = node<Id, primary_node<Token<Tokens::Identifier, LData>>, typename ast_solve<Right>::type>;
    };

    template <Tokens Id, class RData, class Left>
    struct ast_solve<
        node<Id, Left, primary_node<Token<Tokens::Identifier, RData>>>> {
        using type = node<Id, typename ast_solve<Left>::type, primary_node<Token<Tokens::Identifier, RData>>>;
    };

    template <Tokens Id, class LData, class RData>
    struct ast_solve<
        node<Id, primary_node<Token<Tokens::Identifier, LData>>, primary_node<Token<Tokens::Number, RData>>>> {
        using type = node<Id, primary_node<Token<Tokens::Identifier, LData>>, primary_node<Token<Tokens::Number, RData>>>;
    };

    template <Tokens Id, class RData, class LData>
    struct ast_solve<
        node<Id, primary_node<Token<Tokens::Number, LData>>, primary_node<Token<Tokens::Identifier, RData>>>> {
        using type = node<Id, primary_node<Token<Tokens::Number, LData>>, primary_node<Token<Tokens::Identifier, RData>>>;
    };

    template <Tokens Id, class LData, class Right>
    struct ast_solve<
        node<Id, primary_node<Token<Tokens::Number, LData>>, Right>> {
        using type = typename ast_solve<node<Id, primary_node<Token<Tokens::Number, LData>>, typename ast_solve<Right>::type>>::type;
    };

    template <Tokens Id, class RData, class Left>
    struct ast_solve<
        node<Id, Left, primary_node<Token<Tokens::Number, RData>>>> {
        using type = typename ast_solve < node<Id, typename ast_solve<Left>::type, primary_node<Token<Tokens::Number, RData>>>>::type;
    };

    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::Plus, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
      using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value + RData::value>>>;
    };

    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::Minus, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
        using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value - RData::value>>>;
    };

    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::Mul, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
      using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value * RData::value>>>;
    };

    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::Div, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
        using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value / RData::value>>>;
    };

    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::Or, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
        using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value | RData::value>>>;
    };

    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::And, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
        using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value & RData::value>>>;
    };
    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::Xor, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
        using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value ^ RData::value>>>;
    };

    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::DoubleOr, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
        using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value | RData::value>>>;
    };

    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::DoubleAnd, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
        using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value & RData::value>>>;
    };
    template <class LData, class RData>
    struct ast_solve<
        node<Tokens::DoubleXor, primary_node<Token<Tokens::Number, LData>>,
        primary_node<Token<Tokens::Number, RData>>>> {
        using type = primary_node<Token<Tokens::Number, std::integral_constant<std::size_t, LData::value ^ RData::value>>>;
    };
}

template<std::size_t Offset, std::size_t LocalLabelOffset, class LabelList>
struct operands_context {
    static constexpr std::size_t offset = Offset;
    static constexpr std::size_t label_offets = LocalLabelOffset;
    using label_list = LabelList;
};

namespace {
    template <typename... T>
    struct parse_instr_name {
        using value = void;
    };
}  // namespace

template<typename Name, typename P>
struct label_info {
    using name = Name;
    using offset = P;
};

template<typename... T>
struct find_labeloffset;

template<typename Name, class Offset, typename Lname, typename... Data>
struct find_labeloffset<Name, hold<label_info<Lname, Offset>, Data...>> {
    using type = typename find_labeloffset<Name, hold<Data...>>::type;
};

template<typename Name, class Offset, typename... Data>
struct find_labeloffset<Name, hold<label_info<Name, Offset>, Data...>> {
    using type = Offset;
};

template<typename Name>
struct find_labeloffset<Name, hold<>> {
    using type = std::integral_constant<std::size_t, 0>;
};

namespace {
    template<typename... T>
    struct map_nodes;

    template<class Context>
    struct map_nodes<hold<>, Context> {
        using type = hold<>;
        using has_labelref = std::false_type;
    };

    template<typename First, typename... Data, class Context>
    struct map_nodes<hold<First, Data...>, Context> {
        using _first = map_nodes<First, Context>;
        using _second = map_nodes<hold<Data...>, Context>;
        using type = type_list_append_t<hold<typename _first::type>,
            typename _second::type>;

        using has_labelref = std::bool_constant<_first::has_labelref::value || _second::has_labelref::value>;
    };

    template<class Name, class Context>
    struct map_nodes<labelref_node<Token<Tokens::Identifier, Name>>, Context> {
        using type = primary_node<
            Token<Tokens::Number, 
            std::integral_constant<std::size_t, find_labeloffset<Name, typename Context::label_list>::type::value>>>;
        using has_labelref = std::true_type;
    };

    template<class Context>
    struct map_nodes<primary_node<Token<Tokens::Dollar, void>>, Context> {
        using type = primary_node<
            Token<Tokens::Number,
            std::integral_constant<std::size_t, Context::offset>>>;
        using has_labelref = std::false_type;
    };

    template<class Context>
    struct map_nodes<primary_node<Token<Tokens::DoubleDollar, void>>, Context> {
        using type = primary_node<
            Token<Tokens::Number,
            std::integral_constant<std::size_t, Context::label_offets>>>;
        using has_labelref = std::false_type;
    };

    template<class Data, class Context>
    struct map_nodes<primary_node<Token<Tokens::Identifier, Data>>, Context> {
        using type = primary_node<Token<Tokens::Identifier, Data>>;
        using has_labelref = std::false_type;
    };

    template<class Data, class Context>
    struct map_nodes<primary_node<Token<Tokens::Number, Data>>, Context> {
        using type = primary_node<Token<Tokens::Number, Data>>;
        using has_labelref = std::false_type;
    };

    template <class Context, class Data>
    struct map_nodes<unary_node<Tokens::Ptr, Data>, Context> {
        using _first = map_nodes<Data, Context>;
        using type = unary_node<Tokens::Ptr, typename _first::type>;
        using has_labelref = typename _first::has_labelref;
    };

    template <class Context, class Data>
    struct map_nodes<unary_node<Tokens::LeftParen, Data>, Context> {
        using _first = map_nodes<Data, Context>;
        using type = unary_node<Tokens::LeftParen, typename _first::type>;
        using has_labelref = typename _first::has_labelref;
    };

    template <class Context, class Data>
    struct map_nodes<unary_node<Tokens::LeftSquare, Data>, Context> {
        using _first = map_nodes<Data, Context>;
        using type = unary_node<Tokens::LeftSquare, typename _first::type>;
        using has_labelref = typename _first::has_labelref;
    };

    template <class Context, class RegSize, class Data>
    struct map_nodes<unary_node<Tokens::SizeWord, hold<RegSize, Data>>, Context> {
        using _first = map_nodes<Data, Context>;
        using type = unary_node<Tokens::SizeWord, hold<RegSize, typename _first::type>>;
        using has_labelref = typename _first::has_labelref;
    };

    template <class Context, class RegSize, class Data>
    struct map_nodes<unary_node<Tokens::SizeWord, hold<RegSize, unary_node<Tokens::Ptr, Data>>>, Context> {
        using _first = map_nodes<Data, Context>;
        using type = unary_node<Tokens::SizeWord, hold<RegSize, unary_node<Tokens::Ptr, typename _first::type>>>;
        using has_labelref = typename _first::has_labelref;
    };

    template <class Context, Tokens Tok, class Left, class Right>
    struct map_nodes<node<Tok, Left, Right>, Context> {
        using _first = map_nodes<Left, Context>;
        using _second =  map_nodes<Right, Context>;
        using type = node<Tok, typename _first::type, typename _second::type>;
        using has_labelref = std::bool_constant<_first::has_labelref::value || _second::has_labelref::value>;
    };
}

// code gen. first step. Claclulate sizes, label offsets
namespace {

    template <class P, std::size_t Offset = 0>
    struct processed_instr {
      using data = P;
      static constexpr std::size_t offset = Offset;
    };

    template<class P, std::size_t Offset, std::size_t LocalLabelOffset>
    struct for_second_phase_instr {
        using _node = P;
        static constexpr std::size_t label_offset = LocalLabelOffset;
        static constexpr std::size_t offset = Offset;
    };

    template <class Name, class P, std::size_t Offset = 0>
    struct processed_label {
        using name = Name;
        using data = P;
        static constexpr std::size_t offset = Offset;
    };

    template<typename T> struct repack_ptr;

    template<class R, class... S>
    struct repack_ptr<ptr<R, hold<S...>>> {
        using type = ptr<R, S...>;
    };

    template<typename ...T> struct assemble;
    template <typename... T>
    struct assemble_ops {using type = hold<>;};

    template <class Previous, class LocalLabelOffset>
    struct assemble<Previous, LocalLabelOffset, hold<>> {
      using type = hold<>;
      static constexpr std::size_t size = Previous::value;

      using label_list = hold<>;
    };

    template <class Previous, class LocalLabelOffset, class F, class ...Data>
    struct assemble<Previous, LocalLabelOffset, hold<F, Data...>> {
      using _first = assemble<Previous, LocalLabelOffset, F>;
      using _next = std::integral_constant<std::size_t, _first::type::head::offset>;
      using _second = assemble<_next, LocalLabelOffset, hold<Data...>>;

      using type = type_list_append_t<typename _first::type, typename _second::type>;
      using next = typename _second::type;

      static constexpr std::size_t size = _second::size;

      using label_list = type_list_append_t<typename _first::label_list, typename _second::label_list>;

    };

    template <class Previous, class LocalLabelOffset, class Name, class F, class ...Data>
    struct assemble<Previous, LocalLabelOffset, label_node<Name, hold<F, Data...>>> {
      using _first = assemble<Previous, Previous, F>;
      using _next = std::integral_constant<std::size_t, _first::type::head::offset>;
      using _second = assemble<_next, Previous, hold<Data...>>;

      static constexpr std::size_t size = _second::size;
      
      using type = hold<processed_label<Name, type_list_append_t<typename _first::type, typename _second::type>, size>>;
      using next = typename _second::type;
        
      using label_list = type_list_append_t<hold<label_info<typename Name::left::data, Previous>>, typename _first::label_list, typename _second::label_list>;
    };

    template <class Previous, class LocalLabelOffset, class Name>
    struct assemble<Previous, LocalLabelOffset, label_node<Name, hold<>>> {
        static constexpr std::size_t size = 0;

        using type = hold<processed_label<Name, hold<>, Previous::value>>;
        using next = Previous;

        using label_list = hold<label_info<typename Name::left::data, Previous>>;
    };

    template <class Previous, class LocalLabelOffset, class Name, class Ops>
    struct assemble<Previous, LocalLabelOffset, instr_node<primary_node<Token<Tokens::Identifier, Name>>, Ops>> {
      using mapped_args = map_nodes<Ops, operands_context<Previous::value, LocalLabelOffset::value, hold<>>>;
      using args = assemble_ops < Previous, LocalLabelOffset, typename ast_solve<typename mapped_args::type>::type >;
      using __local = parse_instr_name<Name, typename args::type>;
      
      static constexpr std::size_t size =
          Previous::value + typename __local::value{}.size();
      using _local = std::conditional_t<mapped_args::has_labelref::value,
          for_second_phase_instr<
            instr_node<primary_node<Token<Tokens::Identifier, Name>>, Ops>, 
          size, LocalLabelOffset::value>,
          processed_instr<typename __local::value, size>>;
      using type = hold<_local>;

      using label_list = hold<>;
    };

    template <class Previous, class LocalLabelOffset>
    struct assemble_ops<Previous, LocalLabelOffset, hold<>> {
        using type = hold<>;
    };

    template <class Previous, class LocalLabelOffset, class Data>
    struct assemble_ops<Previous, LocalLabelOffset,
                        primary_node<Token<Tokens::Identifier, Data>>> {
      using type = hold<get_reg_t<Data>>;
    };

    template <class Previous, class LocalLabelOffset, class Data>
    struct assemble_ops<Previous, LocalLabelOffset, primary_node<Token<Tokens::Number, Data>>> {
        using type = hold<pack_value<Data::value>>;
    };

    template <class Previous, class LocalLabelOffset, class RegSz, class Data>
    struct assemble_ops<Previous, LocalLabelOffset,
        unary_node<Tokens::SizeWord, hold<RegSz, primary_node<Token<Tokens::Number, Data>>>>> {
        using type = hold<ux<get_reg_sz<RegSz>::value, Data::value>>;
    };

    template <class Previous, class LocalLabelOffset, class Op, class... Ops>
    struct assemble_ops<Previous, LocalLabelOffset, hold<Op, Ops...>> {
        using _first = assemble_ops<Previous, LocalLabelOffset, Op>;
        using _second = assemble_ops<Previous, LocalLabelOffset, hold<Ops...>>;
        using type = type_list_append_t<typename _first::type, typename _second::type>;
    };

    template <class Previous, class LocalLabelOffset, class L, class R>
    struct assemble_ops<Previous, LocalLabelOffset, node<Tokens::Plus, L, R>> {
      using _left = typename assemble_ops<Previous, LocalLabelOffset, L>::type;
      using _right = typename assemble_ops<Previous, LocalLabelOffset, R>::type;
      using type = type_list_append_t<_left, hold<plus>, _right>;
    };

    template <class Previous, class LocalLabelOffset, class L, class R>
    struct assemble_ops<Previous, LocalLabelOffset, node<Tokens::Mul, L, R>> {
      using _left = typename assemble_ops<Previous, LocalLabelOffset, L>::type;
      using _right = typename assemble_ops<Previous, LocalLabelOffset, R>::type;
      using type = type_list_append_t<_left, hold<mul>, _right>;
    };

    template <class Previous, class LocalLabelOffset, class Data>
    struct assemble_ops<Previous, LocalLabelOffset, unary_node<Tokens::Ptr, Data>> {
        using _first = assemble_ops<Previous, LocalLabelOffset, Data>;
        using type = hold<typename repack_ptr<ptr<reg64, typename _first::type>>::type>;
    };

    template <class Previous, class LocalLabelOffset, class RegSz, class Data>
    struct assemble_ops<Previous, LocalLabelOffset, unary_node<Tokens::SizeWord, hold<RegSz, unary_node<Tokens::Ptr, Data>>>> {
        using _first = assemble_ops<Previous, LocalLabelOffset, Data>;
        using type = hold<typename repack_ptr<ptr<RegSz, typename _first::type>>::type>;
    };
}

// second stage
namespace {
    template<typename... T>
    struct solve_assembled {};

    template<class LabelList>
    struct solve_assembled<LabelList, hold<>> {
        using type = hold<>;
    };

    template<class LabelList, class First, class... Data>
    struct solve_assembled<LabelList, hold<First, Data...>> {
        using _first = solve_assembled<LabelList, First>;
        using _second = solve_assembled<LabelList, hold<Data...>>;
        using type = type_list_append_t<typename _first::type, typename _second::type>;
    };
        
    template<class LabelList, class Name, std::size_t N>
    struct solve_assembled<LabelList, processed_label<Name, hold<>, N>> {
        using type = hold<processed_label<Name, hold<>>>;
    };

    template<class LabelList, class Name, class First, class... Data, std::size_t N>
    struct solve_assembled<LabelList, processed_label<Name, hold<First, Data...>, N>> {
        using _first = solve_assembled<LabelList, First>;
        using _second = solve_assembled<LabelList, hold<Data...>>;
        using type = hold<processed_label<Name, type_list_append_t<typename _first::type, typename _second::type>>>;
    };

    template<class LabelList, class P, std::size_t N>
    struct solve_assembled<LabelList, processed_instr<P, N>> {
        using type = hold<processed_instr<P, N>>;
    };

    template<typename... T>
    struct solve_ops;

    template<class LabelList, class Name, class Ops, std::size_t N, std::size_t N0>
    struct solve_assembled<LabelList, for_second_phase_instr<instr_node<primary_node<Token<Tokens::Identifier, Name>>, Ops>, N, N0>> {
        using args = typename assemble_ops<
            std::integral_constant<std::size_t, N>,
            std::integral_constant<std::size_t, N0>, 
            typename ast_solve<
                typename map_nodes<Ops, operands_context<N, N0, LabelList>>::type
            >::type
        >::type;
        
        using __local = parse_instr_name<Name, args>;

        static constexpr std::size_t size =
            N + typename __local::value{}.size();
        using _local = 
            processed_instr<typename __local::value, size>;
        using type = hold<_local>;
    };
}

// collect assembled
namespace {
    template<typename T> struct collect_data;

    template <> struct collect_data<hold<>> {
      using type = std::integer_sequence<uint8_t>;
    };

    template <class Name>
    struct collect_data<processed_label<Name, hold<>>> {
        using type = std::integer_sequence<uint8_t>;
    };
    
    template<class label>
    struct collect_data<hold<label>> {
      using type = typename collect_data<label>::type;
    };

    template<class First, class... Nodes>
    struct collect_data<hold<First, Nodes...>> {
        using type = expand_byte_seq_t<typename collect_data<First>::type, typename collect_data<hold<Nodes...>>::type>;
    };

    template<class Name, class First, class... Nodes>
    struct collect_data<processed_label<Name, hold<First, Nodes...>>> {
        using type = expand_byte_seq_t<typename collect_data<First>::type, typename collect_data<hold<Nodes...>>::type>;
    };
    template<class Data, std::size_t N>
    struct collect_data<processed_instr<Data, N>> {
        using type = Data;
    };
}
template<uint64_t N>
struct make_null_seq {
  using value = expand_byte_seq_v<byte_seq<0x00>, typename make_null_seq<N - 1>::value>;
};

template <>
struct make_null_seq<0> {
  using value = std::integer_sequence<uint8_t>;
};


//PoC Preprocessor
namespace{

  template<typename, typename>
  struct skip_word {};

  template<char ...AStr, char Felem,  char ...Str>
  struct skip_word<char_seq<AStr...>, char_seq<Felem,Str...>> {
    using type = std::conditional_t<
        is_identifier_char_v<Felem>::value,
        typename skip_word<char_seq<AStr..., Felem>, char_seq<Str...>>::type,
        char_seq<AStr...>>;
  };

  template <char... AStr>
  struct skip_word<char_seq<AStr...>, char_seq<>> {
    using type = char_seq<AStr...>;
  };

  template<typename T>
  using skip_word_t = typename skip_word<char_seq<>, T>::type;

  template <typename, typename, typename, char>
  struct split_seq {};

  template <typename... Acc, char... AStr, char First, char... Str, char C>
  struct split_seq<hold<Acc...>, char_seq<AStr...>, char_seq<First, Str...>, C> {
    using type = typename split_seq<hold<Acc...>, char_seq<AStr..., First>,
                                        char_seq<Str...>, C>::type;
  };

  template <typename... Acc, char... AStr, char... Str, char C>
  struct split_seq<hold<Acc...>, char_seq<AStr...>, char_seq<C, Str...>, C> {
    using type = typename split_seq<hold<Acc..., char_seq<AStr...>>, char_seq<>,
                                    char_seq<Str...>, C>::type;
  };

  template <typename... Acc, char... AStr, char C>
  struct split_seq<hold<Acc...>, char_seq<AStr...>, char_seq<>, C> {
    using type = hold<Acc..., char_seq<AStr...>>;
  };

  template <typename... Acc, char C>
  struct split_seq<hold<Acc...>, char_seq<>, char_seq<>, C> {
    using type = hold<Acc...>;
  };

  template <typename T, char C>
  using split_seq_t = typename split_seq<hold<>, char_seq<>, T, C>::type;


  template<typename T, typename Y>
  struct compare{};

  template<char LR, char ...LStr, char ...RStr>
  struct compare<char_seq<LR, LStr...>, char_seq<LR, RStr...>>{
    using type = typename compare<char_seq<LStr...>, char_seq<RStr...>>::type;
  };

  template<char LFirst, char ...LStr, char RFirst, char ...RStr>
  struct compare<char_seq<LFirst, LStr...>, char_seq<RFirst, RStr...>>{
    using type = std::false_type;
  };

  template<char RFirst, char ...RStr>
  struct compare<char_seq<>, char_seq<RFirst, RStr...>>{
    using type = std::bool_constant<!is_identifier_char_v<RFirst>::value>;
  };
  
  template<>
  struct compare<char_seq<>, char_seq<>>{
    using type = std::true_type;
  };

  template<typename T>
  struct compare<T, char_seq<>>{
   using type = std::false_type; 
  };

  template<typename T, typename Y>
  using compare_t = typename compare<T, Y>::type;

  template<typename, typename> struct char_expand{};

  template<char ...LStr, char ...RStr> struct char_expand<char_seq<LStr...>, char_seq<RStr...>>{
    using type = char_seq<LStr..., RStr...>;
  };

  template<typename T, typename Y> using char_expand_t = typename char_expand<T, Y>::type;

  template<typename T>
  using parse_columns_t = split_seq_t<T, ','>;

  template<typename ...T> struct find_and_replace{};

  
  template <char... FindStr, typename ReplaceStr, char... AStr, char FChar,
            char... Str>
  struct find_and_replace<char_seq<FindStr...>, ReplaceStr, char_seq<AStr...>,
                          char_seq<FChar, Str...>> {
  
    using type = std::conditional_t<(!is_identifier_char_v<FChar>::value) && std::is_same<char_seq<FindStr...>, skip_word_t<char_seq<Str...>>>::value, 
            typename find_and_replace<char_seq<FindStr...>, ReplaceStr, char_expand_t<char_seq<AStr..., FChar>, ReplaceStr>, skip_for_t<char_seq<Str...>, sizeof...(FindStr)>>::type,
            typename find_and_replace<char_seq<FindStr...>, ReplaceStr, char_seq<AStr..., FChar>, char_seq<Str...>>::type>;
  };

  template <char... FindStr, char... ReplaceStr, char... AStr>
  struct find_and_replace<char_seq<FindStr...>, char_seq<ReplaceStr...>,
                          char_seq<AStr...>, char_seq<>> {
    using type = char_seq<AStr...>;
  };

  template<typename T, typename Y, typename Z>
  using find_and_replace_t = typename find_and_replace<T, Y, char_seq<>, Z>::type;

  template<typename ...T>
  struct find_and_delete {};

  template<typename ...T>
  using find_and_delete_t = typename find_and_delete<T...>::type;

  template <typename ...ADefs, class Def, typename... Defs, char ...Str>
  struct find_and_delete<hold<ADefs...>, hold<Def, Defs...>,
                         char_seq<Str...>> {
    using type = 
        std::conditional_t<std::is_same<typename Def::head, char_seq<Str...>>::value, 
            find_and_delete_t<hold<ADefs...>, hold<Defs...>, char_seq<Str...>>,
            find_and_delete_t<hold<ADefs..., Def>, hold<Defs...>, char_seq<Str...>>>;
  };

  template <typename... ADefs, char... Str>
  struct find_and_delete<hold<ADefs...>, hold<>, char_seq<Str...>> {
    using type = hold<ADefs...>;
  };

  template <typename, typename, typename, std::size_t>
  struct fucking_smth_impl {};

  template <typename Arg, char... AStr, char... Str, std::size_t N>
  struct fucking_smth_impl<Arg, char_seq<AStr...>, char_seq<'%', char(N + '0'), Str...>, N> {
    using type = typename fucking_smth_impl<Arg, char_expand_t<char_seq<AStr...>, Arg>, char_seq<Str...>, N>::type;
  }; 
  
  template <typename Arg, char... AStr, char FStr, char... Str, std::size_t N>
  struct fucking_smth_impl<Arg, char_seq<AStr...>, char_seq<FStr, Str...>, N> {
    using type = typename fucking_smth_impl<Arg, char_seq<AStr..., FStr>, char_seq<Str...>, N>::type;
  };

  template <typename Arg, char... AStr, std::size_t N>
  struct fucking_smth_impl<Arg, char_seq<AStr...>, char_seq<>, N> {
    using type = char_seq<AStr...>;
  };

  template<typename, typename, std::size_t>
  struct fucking_smth{};

  template<typename Arg, typename... Args, char ...Str, std::size_t N>
  struct fucking_smth<hold<Arg, Args...>, char_seq<Str...>, N> {
      using type = typename fucking_smth<hold<Args...>, typename fucking_smth_impl<Arg, char_seq<>, char_seq<Str...>, N>::type, N + 1>::type;
  };

  template <char... Str, std::size_t N>
  struct fucking_smth<hold<>, char_seq<Str...>,  N> {
    using type = char_seq<Str...>;
  };
  
  template<typename, typename, size_t>
  struct process_def {};

  template<typename Def, typename ...Defs, char ...Str, size_t N>
  struct process_def<hold<Def, Defs...>, char_seq<Str...>, N> {
      using type = typename process_def<hold<Defs...>, find_and_replace_t<Def, char_seq<'%', char(N + '0')>, char_seq<Str...>>, N + 1>::type;
  };

  template <typename... Defs, char... Str, size_t N>
  struct process_def<hold<char_seq<>, Defs...>, char_seq<Str...>, N> {
    using type = typename process_def<hold<Defs...>, char_seq<Str...>, N + 1>::type;
  };

  template <char... Str, size_t N>
  struct process_def<hold<>, char_seq<Str...>, N> {
    using type = char_seq<Str...>;
  };

  template<typename T, typename Y>
  using process_def_t = typename process_def<T, Y, 0>::type;

  template<char ...Str>
  using parse_args = parse_columns_t<skip_spaces_t<collect_until_t<skip_until_t<char_seq<Str...>, is_same_as<'('>>, is_same_as<')'>>>>;

  template <typename, typename, typename> struct paste_defs_impl {};

  template<typename FDef, char ...AStr, char FStr, char ...Str>
  struct paste_defs_impl<FDef, char_seq<AStr...>, char_seq<FStr, Str...>>{
    using type = std::conditional_t<
        compare_t<typename FDef::head, char_seq<FStr, Str...>>::value &&
            (parse_args<Str...>::sz == FDef::tail::tail::head::sz),
     typename paste_defs_impl<FDef, 
       char_expand_t<char_seq<AStr...>, typename fucking_smth<parse_args<Str...>, typename FDef::tail::head, 0>::type>, 
       skip_for_t<char_seq<FStr, Str...>, typename FDef::head{}.size() +  collect_until_t<char_seq<Str...>, is_same_as<')'>>{}.size()>>::type,
     typename paste_defs_impl<FDef, char_seq<AStr..., FStr>, char_seq<Str...>>::type>;
  };
  
  template<typename FDef, char ...AStr>
  struct paste_defs_impl<FDef, char_seq<AStr...>, char_seq<>>{
      using type = char_seq<AStr...>;
  };

  template<typename ...T>
  using paste_defs_impl_t = typename paste_defs_impl<T...>::type;

  template <typename, typename> struct paste_defs {};

  template<typename FDef, typename ...Defs, char ...Str>
  struct paste_defs<hold<FDef, Defs...>, char_seq<Str...>>{
    using type = typename paste_defs<hold<Defs...>, paste_defs_impl_t<FDef, char_seq<>, char_seq<Str...>>>::type;
  };

  template<char ...Str>
  struct paste_defs<hold<>, char_seq<Str...>>{
    using type = char_seq<Str...>;
  };

  template<typename ...T> using paste_defs_t = typename paste_defs<T...>::type;

  template<typename ...T> struct parse_macro_def{};

  template<char... Str>
  struct parse_macro_def<char_seq<Str...>>{
    using name = skip_spaces_t<collect_until_t<char_seq<Str...>, is_same_as<'('>>>;
    using args = parse_args<Str...>;
    using def  = collect_until_t<skip_until_t<char_seq<Str...>, is_same_as<')'>>, is_same_as<';'>>;

    using processed_def = process_def_t<args, def>;

    using type = hold<name, processed_def, args>;
  };

  template <typename... T> struct parse_macro_xdef {};

  template <typename ...Defs, char... Str>
  struct parse_macro_xdef<hold<Defs...>, char_seq<Str...>> {
    using name = skip_spaces_t<collect_until_t<char_seq<Str...>, is_same_as<'('>>>;
    using args = parse_args<Str...>;
    using def = paste_defs_t<hold<Defs...>, collect_until_t<skip_until_t<char_seq<Str...>, is_same_as<')'>>, is_same_as<';'>>>;

    using processed_def = process_def_t<args, def>;

    using type = hold<name, processed_def, args>;
  };
   
  template <typename... T> struct parse_macro_undef {};

  template <typename... Defs, char... Str>
  struct parse_macro_undef<hold<Defs...>, char_seq<Str...>> {
    using name = skip_spaces_t<collect_until_t<char_seq<Str...>, is_same_as<';'>>>;

    using type = find_and_delete_t<hold<>, hold<Defs...>, name>;
  };

  template <typename... T> struct parse_macro_alias {};

  template <typename... Defs, char... Str>
  struct parse_macro_alias<hold<Defs...>, char_seq<Str...>> {
    using name = skip_spaces_t<collect_until_t<skip_until_t<char_seq<Str...>, is_same_as<' '>>, is_same_as<' '>>>;

    using alias_name = skip_spaces_t<collect_until_t<char_seq<Str...>, is_same_as<';'>>>;

    using type = hold<name, alias_name, void>;
  };

  template<typename, typename, typename>
  struct asm_preprocess{
    using type = void;
  };
  
  template<typename ...Defs, char ...acc_str, char Head, char ...str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<Head, str...>>{
    using type = typename asm_preprocess<hold<Defs...>, char_seq<acc_str..., Head>, char_seq<str...>>::type;
  };

  template<typename ...Defs, char ...acc_str, char ...str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<'%','d', 'e', 'f', 'i', 'n', 'e', ' ', str...>>{
    using type = typename asm_preprocess<hold<typename parse_macro_def<char_seq<str...>>::type, Defs...>, char_seq<acc_str...>, skip_until_t<char_seq<str...>, is_same_as<';'>>>::type;
  };

  template<typename ...Defs, char ...acc_str, char ...str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<'%','x', 'd', 'e', 'f', 'i', 'n', 'e', ' ', str...>>{
    using type = typename asm_preprocess<hold<typename parse_macro_xdef<hold<Defs...>, char_seq<str...>>::type, Defs...>, char_seq<acc_str...>, skip_until_t<char_seq<str...>, is_same_as<';'>>>::type;
  };

  template<typename ...Defs, char ...acc_str, char ...str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<'%', 'u', 'n', 'd', 'e', 'f', ' ', str...>>{
    using type = typename asm_preprocess<typename parse_macro_undef<hold<Defs...>, char_seq<str...>>::type, char_seq<acc_str...>, skip_until_t<char_seq<str...>, is_same_as<';'>>>::type;
  };

  
  template<typename ...Defs, char ...acc_str, char ...str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<'%', 'a', 'l', 'i', 'a', 's', ' ', str...>>{
    using type = typename asm_preprocess<typename parse_macro_alias<hold<Defs...>, char_seq<str...>>::type, char_seq<acc_str...>, skip_until_t<char_seq<str...>, is_same_as<';'>>>::type;
  };

  template<typename ...Defs, char ...acc_str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<>>{
    using type = paste_defs_t<hold<Defs...>, char_seq<acc_str...>>;
  };

  template<typename ...Defs, char ...acc_str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<'\0'>>:
    asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<>>{};


}


//PoC "dx" thing
namespace {
    template<std::size_t Size, class... T> struct calculate_size: std::integral_constant<std::size_t, 0>{};
  
    template<std::size_t Size, class T, class... Y>
    struct calculate_size<Size, T, Y...>: calculate_size<Size + typename T::value{}.size(), Y...>{};

    template<std::size_t Size, class T>
    struct calculate_size<Size, T>: std::integral_constant<std::size_t, Size + typename T::value{}.size()>{};


    template<std::size_t, typename... T> struct _dx_impl;
    template<typename... T> struct _dx;

    template <char... str, typename... T>
    struct parse_instr_name<char_seq<'d','b', str...>, hold<T...>> {
        using value = typename _dx_impl<1, T...>::value;
    };

    template <char... str, typename... T>
    struct parse_instr_name<char_seq<'d','w', str...>, hold<T...>> {
        using value = typename _dx_impl<2, T...>::value;
    };

    template <char... str, typename... T>
    struct parse_instr_name<char_seq<'d','d', str...>, hold<T...>> {
        using value = typename _dx_impl<4, T...>::value;
    };

    template <char... str, typename... T>
    struct parse_instr_name<char_seq<'d','q', str...>, hold<T...>> {
        using value = typename _dx_impl<8, T...>::value;
    };

    template <char... str, typename... T>
    struct parse_instr_name<char_seq<'d','t', str...>, hold<T...>> {
        using value = typename _dx_impl<10, T...>::value;
    };
    
    template <char... str, typename... T>
    struct parse_instr_name<char_seq<'d','o', str...>, hold<T...>> {
        using value = typename _dx_impl<16, T...>::value;
    };
    
    template <char... str, typename... T>
    struct parse_instr_name<char_seq<'d','y', str...>, hold<T...>> {
        using value = typename _dx_impl<32, T...>::value;
    };

    template<std::size_t N, class V, class... T> 
    struct _dx_impl<N, V, T...>{
        using value = expand_byte_seq_v<typename _dx<V>::value, typename _dx<T...>::value, typename make_null_seq<N - (calculate_size<0, V, T...>::value % N)>::value>;
    };

    template<class V, class... T> 
    struct _dx_impl<1, V, T...>{
        using value = expand_byte_seq_v<typename _dx<V>::value, typename _dx<T...>::value>;
    };
    template<class V, class... T> 
    struct _dx<V, T...>{
        using value = expand_byte_seq_v<typename _dx<V>::value, typename _dx<T...>::value>;
    };

    template<class V> 
    struct _dx<V>{
        using value = typename V::value;
    };

    template<> 
    struct _dx<>{
        using value = byte_seq<>;
    };
  
}

template <std::size_t N, const char (&s)[N], typename T>  
struct make_char_sequence_impl;

template <std::size_t N, const char (&s)[N], std::size_t... i>
struct make_char_sequence_impl<N, s, std::index_sequence<i...>> {
  using type = char_seq<s[i]...>;
};

template <std::size_t N, const char (&Input)[N]>
using make_char_sequence = typename make_char_sequence_impl<
    N, Input, std::make_index_sequence<N>>::type;

}

#define ctasm(_str)                                                  \
  ([]{                                                               \
    using namespace ctasm;                                           \
    static constexpr const char literal[] = (_str);                  \
    using c_seq = make_char_sequence<sizeof(literal), literal>;      \
    using toks = typename lex<0, c_seq{}.size(), c_seq>::type;       \
    using ast = typename parse_global<toks>::type;                   \
    using first_phase = assemble<std::integral_constant<std::size_t, 0>, \
                                std::integral_constant<std::size_t, 0>, ast>; \
    using assembled = solve_assembled<typename first_phase::label_list, \
                                      typename first_phase::type>::type; \
    using raw_out = collect_data<assembled>::type; \
    return seq_to_arr<raw_out>::value ; \
  }())

#include "ctasm_emit.hpp"
#endif