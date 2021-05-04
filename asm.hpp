#pragma once

#include <limits>
#include <iostream>
#include <string>
#include <functional>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>
#include <array>
#include <algorithm>

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
struct u16_8{};

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
using is_67h_needed = 
        typename std::conditional_t<std::is_same<T, reg32>::value, std::is_same<regs_bit, reg64>,
        typename std::conditional_t<std::is_same<T, reg16>::value, std::is_same<regs_bit, reg32>,
        typename std::conditional_t<std::is_same<T, reg8>::value,  std::is_same<regs_bit, reg16>,
                       std::false_type>>>::type;

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


template<typename>
struct byte_67h {
  using value = std::integer_sequence<uint8_t>;
};

template <>
struct byte_67h<std::true_type> {
  using value = byte_seq<0x67>;
};

namespace {
    template <char first_letter, char... chars> struct first_elem {
      static constexpr auto value = first_letter;
    };

    template <char first_letter>
    struct first_elem<first_letter> {
      static constexpr auto value = first_letter;
    };


    template <typename... T> struct get_elem { static constexpr char value = 0; };
    template <char f, char... chars> struct get_elem<char_seq<f, chars...>> {
      static constexpr char value = f;
    };

  namespace {
      template <char a, char... b> struct _slice_ {
        using value = std::integer_sequence<char, b...>;
      };
      template <std::size_t N, typename T> struct _first_elements {};

      template <std::size_t end, char... chars>
      struct _first_elements<end, std::integer_sequence<char, chars...>> {
        using value =
            typename _first_elements<end - 1,
                                    typename _slice_<chars...>::value>::value;
      };

      template <char... chars>
      struct _first_elements<0, std::integer_sequence<char, chars...>> {
        using value = typename std::integer_sequence<char, chars...>;
      };

      template <std::size_t end, char... chars>
      struct slice
          : _first_elements<end, std::integer_sequence<char, chars...>> {};

  } // namespace

  namespace {
      namespace {

          template <uint64_t x, uint64_t y>
          struct _pow: _pow<x * x, y - 1>{};

          template <uint64_t x>
          struct _pow<x, 1>: std::integral_constant<uint64_t, x>{};

          template<typename, char> struct skip_until{};

          template<char el, char... str, char c>
          struct skip_until<char_seq<el, str...>, c>: skip_until<char_seq<str...>, c>{};

          template<char... str, char c>
          struct skip_until<char_seq<c, str...>, c>{
            using type = char_seq<str...>;
          };

          template<char c>
          struct skip_until<char_seq<>, c>{
            using type = char_seq<>;
          };


          template<typename T, char c>
          using skip_until_t = typename skip_until<T, c>::type;


          template<typename, std::size_t> struct _skip_for{};

          template<char First, char ...Str, std::size_t N>
          struct _skip_for<char_seq<First, Str...>, N>{
            using type = std::conditional_t<N != 0, typename _skip_for<char_seq<Str...>, (N - 1)>::type, char_seq<First, Str...>>;
          };
          
          template<std::size_t N>
          struct _skip_for<char_seq<>, N>{
            using type = char_seq<>;
          };

          
          template<typename T, std::size_t N> using skip_for_t = typename _skip_for<T, N>::type;

          template<typename, typename, char> struct collect_until{};
          template<char ...Acc, char First, char ...Str, char V>
          struct collect_until<char_seq<Acc...>, char_seq<First, Str...>, V> :
              collect_until<char_seq<Acc..., First>, char_seq<Str...>, V>{};

          template<char ...Acc, char ...Str, char V>
          struct collect_until<char_seq<Acc...>, char_seq<V, Str...>, V>{
            using type = char_seq<Acc...>;
          };

          template<char ...Acc, char V>
          struct collect_until<char_seq<Acc...>, char_seq<>, V>{
            using type = char_seq<Acc...>;
          };

          template<typename T, char c>
          using collect_until_t = typename collect_until<null_sequence<char>, T, c>::type;


          namespace{
            template <typename T, typename Y, char Char> struct skip_c {};

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
          }

          template <typename T, typename Y> using skip_spaces = skip_c<T, Y, ' '>;
          template <typename T, typename Y> using skip_underscores = skip_c<T, Y, '_'>;

          template <typename T> using skip_spaces_t = typename skip_c<char_seq<>, T, ' '>::value;
          template <typename T> using skip_underscores_t = typename skip_c<char_seq<>, T, '_'>::value;

          template <typename T, uint64_t N>
          struct parse_num_impl: std::integral_constant<uint64_t, 0>{};

          template <char first, char... str, uint64_t N> 
          struct parse_num_impl<char_seq<first, str...>, N> {
            static constexpr uint64_t value =
                parse_num_impl<char_seq<first>, N>::value * (_pow<N, sizeof...(str)>::value) +
                                        parse_num_impl<char_seq<str...>, N>::value;
          };

          template <char first, uint64_t N> 
          struct parse_num_impl<char_seq<first>, N> {
            static constexpr uint64_t value = ((first >= 'A') && (first <= 'F')) ? (first - 'A' + 10) : (first - '0');
          };

          template <typename T>
          struct parse_num: parse_num_impl<T, 10> {};

          template <char... str> 
          struct parse_num<char_seq<str...>>: parse_num_impl<char_seq<str...>, 10> {};

          template <char... str> 
          struct parse_num<char_seq<'0', 'd', str...>>: parse_num_impl<char_seq<str...>, 10> {};

          template <char... str> 
          struct parse_num<char_seq<'0', 'c', str...>>: parse_num_impl<char_seq<str...>, 16> {};

          template <char... str> 
          struct parse_num<char_seq<'0', 'x', str...>>: parse_num_impl<char_seq<str...>, 16> {};

          template <char... str> 
          struct parse_num<char_seq<'0', 'o', str...>>: parse_num_impl<char_seq<str...>,  8>  {};

          template <char... str> 
          struct parse_num<char_seq<'0', 'q', str...>>: parse_num_impl<char_seq<str...>,  8> {};

          template <char... str> 
          struct parse_num<char_seq<'0', 'b', str...>>: parse_num_impl<char_seq<str...>,  2> {};

          template <char... str> 
          struct parse_num<char_seq<'0', 'y', str...>>: parse_num_impl<char_seq<str...>,  2> {};

          template <char... str> 
          struct parse_num<char_seq<'0', 'x', 'c', str...>>: parse_num_impl<char_seq<str...>, 16>{};

          template <char... str> 
          struct parse_num<char_seq<'0', 'h', 'c', str...>>: parse_num_impl<char_seq<str...>, 16>{};


          template<uint64_t N>
          struct make_null_seq {
            using value = expand_byte_seq_v<byte_seq<0x00>, typename make_null_seq<N - 1>::value>;
          };

          template <>
          struct make_null_seq<0> {
            using value = std::integer_sequence<uint8_t>;
          };

          template<typename> struct char_seq_to_v: std::integral_constant<std::size_t, 0>{};

          template<char First, char... Chars> 
          struct char_seq_to_v<char_seq<First, Chars...>>:
            std::integral_constant<std::size_t, ((std::uint64_t)First << (8 * sizeof...(Chars))) + char_seq_to_v<char_seq<Chars...>>::value>{};

          template<> 
          struct char_seq_to_v<char_seq<>>:
            std::integral_constant<std::size_t,  0>{};

          template<typename, typename>
          struct _parse_str_impl{};

          template<char ...Acc, char First, char ...Chars>
          struct _parse_str_impl<char_seq<Acc...>, char_seq<First, Chars...>>{
            using type = typename _parse_str_impl<char_seq<Acc..., First>, char_seq<Chars...>>::type;
          };

          template<char ...Acc, char ...Chars>
          struct _parse_str_impl<char_seq<Acc...>, char_seq<'\'', Chars...>>{
            using type = char_seq<Acc...>;
          };
          
          template<typename>
          struct parse_str {};
          
          template<char ...Chars>
          struct parse_str<char_seq<Chars...>>{
            using content = typename _parse_str_impl<char_seq<>, char_seq<Chars...>>::type;
            using type =  std::conditional_t<content{}.size() == 0, ub<0x00>,
                          std::conditional_t<content{}.size() == 1, ub<static_cast<uint8_t >(char_seq_to_v<content>::value)>,
                          std::conditional_t<content{}.size() == 2, uw<static_cast<uint16_t>(char_seq_to_v<content>::value)>,
                          std::conditional_t<content{}.size() <= 4, ud<static_cast<uint32_t>(char_seq_to_v<content>::value)>,
                          std::conditional_t<content{}.size() <= 8, uq<static_cast<uint64_t>(char_seq_to_v<content>::value)>, 
                          void>>>>>;
          };

          template<typename T>
          using parse_str_t = typename parse_str<T>::type;

          template <typename T> struct rec_reg { using type = void; };

          template <> struct rec_reg<char_seq<'a', 'l'>> { using type = al; };
          template <> struct rec_reg<char_seq<'c', 'l'>> { using type = cl; };
          template <> struct rec_reg<char_seq<'d', 'l'>> { using type = dl; };
          template <> struct rec_reg<char_seq<'b', 'l'>> { using type = bl; };
          template <> struct rec_reg<char_seq<'a', 'h'>> { using type = ah; };
          template <> struct rec_reg<char_seq<'c', 'h'>> { using type = ch; };
          template <> struct rec_reg<char_seq<'d', 'h'>> { using type = dh; };
          template <> struct rec_reg<char_seq<'b', 'h'>> { using type = bh; };
          
          template <> struct rec_reg<char_seq<'r', '8', 'l'>>      { using type = r8l;  };
          template <> struct rec_reg<char_seq<'r', '9', 'l'>>      { using type = r9l;  };
          template <> struct rec_reg<char_seq<'r', '1', '0', 'l'>> { using type = r10l; };
          template <> struct rec_reg<char_seq<'r', '1', '1', 'l'>> { using type = r11l; };
          template <> struct rec_reg<char_seq<'r', '1', '2', 'l'>> { using type = r12l; };
          template <> struct rec_reg<char_seq<'r', '1', '3', 'l'>> { using type = r13l; };
          template <> struct rec_reg<char_seq<'r', '1', '4', 'l'>> { using type = r14l; };
          template <> struct rec_reg<char_seq<'r', '1', '5', 'l'>> { using type = r15l; };

          template <> struct rec_reg<char_seq<'a', 'x'>> { using type = ax; };
          template <> struct rec_reg<char_seq<'b', 'x'>> { using type = bx; };
          template <> struct rec_reg<char_seq<'c', 'x'>> { using type = cx; };
          template <> struct rec_reg<char_seq<'d', 'x'>> { using type = dx; };
          template <> struct rec_reg<char_seq<'s', 'i'>> { using type = si; };
          template <> struct rec_reg<char_seq<'d', 'i'>> { using type = di; };
          template <> struct rec_reg<char_seq<'s', 'p'>> { using type = sp; };
          template <> struct rec_reg<char_seq<'b', 'p'>> { using type = bp; };

          template <> struct rec_reg<char_seq<'r', '8', 'w'>>      { using type = r8w;  };
          template <> struct rec_reg<char_seq<'r', '9', 'w'>>      { using type = r9w;  };
          template <> struct rec_reg<char_seq<'r', '1', '0', 'w'>> { using type = r10w; };
          template <> struct rec_reg<char_seq<'r', '1', '1', 'w'>> { using type = r11w; };
          template <> struct rec_reg<char_seq<'r', '1', '2', 'w'>> { using type = r12w; };
          template <> struct rec_reg<char_seq<'r', '1', '3', 'w'>> { using type = r13w; };
          template <> struct rec_reg<char_seq<'r', '1', '4', 'w'>> { using type = r14w; };
          template <> struct rec_reg<char_seq<'r', '1', '5', 'w'>> { using type = r15w; };

          template <> struct rec_reg<char_seq<'e', 'a', 'x'>> { using type = eax; };
          template <> struct rec_reg<char_seq<'e', 'b', 'x'>> { using type = ebx; };
          template <> struct rec_reg<char_seq<'e', 'c', 'x'>> { using type = ecx; };
          template <> struct rec_reg<char_seq<'e', 'd', 'x'>> { using type = edx; };
          template <> struct rec_reg<char_seq<'e', 's', 'i'>> { using type = esi; };
          template <> struct rec_reg<char_seq<'e', 'd', 'i'>> { using type = edi; };
          template <> struct rec_reg<char_seq<'e', 's', 'p'>> { using type = esp; };
          template <> struct rec_reg<char_seq<'e', 'b', 'p'>> { using type = ebp; };

          template <> struct rec_reg<char_seq<'z', 'a', 'x'>> { using type = zax; };
          template <> struct rec_reg<char_seq<'z', 'b', 'x'>> { using type = zbx; };
          template <> struct rec_reg<char_seq<'z', 'c', 'x'>> { using type = zcx; };
          template <> struct rec_reg<char_seq<'z', 'd', 'x'>> { using type = zdx; };
          template <> struct rec_reg<char_seq<'z', 's', 'i'>> { using type = zsi; };
          template <> struct rec_reg<char_seq<'z', 'd', 'i'>> { using type = zdi; };
          template <> struct rec_reg<char_seq<'z', 's', 'p'>> { using type = zsp; };
          template <> struct rec_reg<char_seq<'z', 'b', 'p'>> { using type = zbp; };

          template <> struct rec_reg<char_seq<'r', '8', 'd'>>      { using type = r8d;  };
          template <> struct rec_reg<char_seq<'r', '9', 'd'>>      { using type = r9d;  };
          template <> struct rec_reg<char_seq<'r', '1', '0', 'd'>> { using type = r10d; };
          template <> struct rec_reg<char_seq<'r', '1', '1', 'd'>> { using type = r11d; };
          template <> struct rec_reg<char_seq<'r', '1', '2', 'd'>> { using type = r12d; };
          template <> struct rec_reg<char_seq<'r', '1', '3', 'd'>> { using type = r13d; };
          template <> struct rec_reg<char_seq<'r', '1', '4', 'd'>> { using type = r14d; };
          template <> struct rec_reg<char_seq<'r', '1', '5', 'd'>> { using type = r15d; };

          template <> struct rec_reg<char_seq<'r', 'a', 'x'>> { using type = rax; };
          template <> struct rec_reg<char_seq<'r', 'b', 'x'>> { using type = rbx; };
          template <> struct rec_reg<char_seq<'r', 'c', 'x'>> { using type = rcx; };
          template <> struct rec_reg<char_seq<'r', 'd', 'x'>> { using type = rdx; };
          template <> struct rec_reg<char_seq<'r', 's', 'i'>> { using type = rsi; };
          template <> struct rec_reg<char_seq<'r', 'd', 'i'>> { using type = rdi; };
          template <> struct rec_reg<char_seq<'r', 's', 'p'>> { using type = rsp; };
          template <> struct rec_reg<char_seq<'r', 'b', 'p'>> { using type = rbp; };

          template <> struct rec_reg<char_seq<'r', '8'>>      { using type = r8;  };
          template <> struct rec_reg<char_seq<'r', '9'>>      { using type = r9;  };
          template <> struct rec_reg<char_seq<'r', '1', '0'>> { using type = r10; };
          template <> struct rec_reg<char_seq<'r', '1', '1'>> { using type = r11; };
          template <> struct rec_reg<char_seq<'r', '1', '2'>> { using type = r12; };
          template <> struct rec_reg<char_seq<'r', '1', '3'>> { using type = r13; };
          template <> struct rec_reg<char_seq<'r', '1', '4'>> { using type = r14; };
          template <> struct rec_reg<char_seq<'r', '1', '5'>> { using type = r15; };

          template <char... chars> struct rec_reg<char_seq<'s', 't', chars...>> {
            using type = st<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };
          template <char... chars> struct rec_reg<char_seq<'m', 'm', chars...>> {
            using type = mm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };
          template <char... chars> struct rec_reg<char_seq<'x', 'm', 'm', chars...>> {
            using type = xmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };
          template <char... chars> struct rec_reg<char_seq<'y', 'm', 'm', chars...>> {
            using type = ymm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };
          template <char... chars> struct rec_reg<char_seq<'z', 'm', 'm', chars...>> {
            using type = zmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };

          template <char... chars> struct rec_reg<char_seq<'t', 'm', 'm', chars...>> {
            using type = tmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };

          template<typename T>
          using rec_reg_t = typename rec_reg<T>::type;

          template<typename...>
          struct parse_reg_or_num {};

          template <class ...T, typename reg_sz, char... chars>
          struct parse_reg_or_num<hold<T...>, reg_sz, char_seq<chars...>>{
              using value = 
                  typename std::conditional_t<std::is_same<reg_sz, reg8>::value,  ub<uint8_t (parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same<reg_sz, reg16>::value, uw<uint16_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same<reg_sz, reg32>::value, ud<uint32_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same<reg_sz, reg64>::value, uq<uint64_t(parse_num<char_seq<chars...>>::value)>, void
                  >>>>;
          };

          template <class ...T, typename reg_sz, char... chars>
          struct parse_reg_or_num<hold<T...>, reg_sz, char_seq<'-', chars...>>{
              using value = 
                  typename std::conditional_t<std::is_same<reg_sz, reg8>::value,  ib<-int8_t (parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same<reg_sz, reg16>::value, iw<-int16_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same<reg_sz, reg32>::value, id<-int32_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same<reg_sz, reg64>::value, iq<-int64_t(parse_num<char_seq<chars...>>::value)>, void
                  >>>>;
          };

          template<std::size_t N, typename T>
          using _pack_value_impl = std::conditional_t<(std::numeric_limits<T>::min() <= N) && (N <= std::numeric_limits<T>::max()), std::true_type, std::false_type>;
          template<std::size_t N>
          using pack_value = std::conditional_t<_pack_value_impl<N, uint8_t >::value, ub<static_cast<uint8_t >(N)>,
                             std::conditional_t<_pack_value_impl<N, uint16_t>::value, uw<static_cast<uint16_t>(N)>, 
                             std::conditional_t<_pack_value_impl<N, uint32_t>::value, ud<static_cast<uint32_t>(N)>,
                             std::conditional_t<_pack_value_impl<N, uint64_t>::value, uq<static_cast<uint64_t>(N)>, 
                             void>>>>;

          template<typename, typename> struct fucking_struct_impl{};
          template<typename T, typename Y, uint8_t N> struct fucking_struct_impl<hold<T, ub<N>>, Y>: std::false_type{};
          template<typename T, uint8_t N> struct fucking_struct_impl<hold<T, ub<N>>, T>: std::true_type{};

          template<typename> struct fucking_unpack{};
          template<typename T, uint8_t N> struct fucking_unpack<hold<T, ub<N>>>{
            using value = ub<N>;
          };

          template<typename, typename> struct fucking_struct{
            using value = void;
          };

          template<class T, class ...Y, char ...str>
          struct fucking_struct<hold<T, Y...>, char_seq<str...>>{
            using value = 
              typename std::conditional_t<
                fucking_struct_impl<T, char_seq<str...>>::value, 
                  typename fucking_unpack<T>::value, typename fucking_struct<hold<Y...>, char_seq<str...>>::value>;
          };

          template<class T,  char ...str>
          struct fucking_struct<hold<T>, char_seq<str...>>{
            using value = 
              typename std::conditional_t<
                fucking_struct_impl<T, char_seq<str...>>::value, typename fucking_unpack<T>::value, void>;
          };

          template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<chars...>>{
              using value =  
                  typename std::conditional_t<!(first_elem<chars...>::value <= '9' && first_elem<chars...>::value >= '0'),
                  typename std::conditional_t<std::is_same<rec_reg_t<char_seq<chars...>>, void>::value, typename fucking_struct<hold<T...>, char_seq<chars...>>::value , rec_reg_t<char_seq<chars...>>>,
                                          pack_value<parse_num<char_seq<chars...>>::value>>;
          };

          template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<'-', chars...>>{
              using value =  
                  typename std::conditional_t<!(first_elem<chars...>::value <= '9' && first_elem<chars...>::value >= '0'),
                                          rec_reg_t<char_seq<chars...>>,
                                          pack_value<-parse_num<char_seq<chars...>>::value>>;
          };

          template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<'?', chars...>> {
            using value = ub<0x00>;
          };

          template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<'\'', chars...>> {
            using value = parse_str_t<char_seq<chars...>>;
          };


          template<typename ...T>
          using parse_reg_or_num_v = typename parse_reg_or_num<T...>::value;

          template <typename... T> struct parse_instr_ptr {};
        
          template <class ...T, typename... acc_types, char... acc_str, char... str>
          struct parse_instr_ptr<hold<T...>, hold<acc_types...>, char_seq<acc_str...>, char_seq<'*', str...>> {
            using value = typename parse_instr_ptr<hold<T...>, hold<acc_types..., parse_reg_or_num_v<hold<T...>, char_seq<acc_str...>>, mul>, null_sequence<char>, char_seq<str...>>::value;
          };

          template <class ...T, typename... acc_types, char... acc_str, char... str>
          struct parse_instr_ptr<hold<T...>, hold<acc_types...>, char_seq<acc_str...>, char_seq<'+', str...>> {
            using value = typename parse_instr_ptr<hold<T...>, hold<acc_types..., parse_reg_or_num_v<hold<T...>, char_seq<acc_str...>>, plus>, null_sequence<char>, char_seq<str...>>::value;
          };

          template <class ...T, typename... acc_types, char... acc_str, char... str>
          struct parse_instr_ptr<hold<T...>, hold<acc_types...>, char_seq<acc_str...>, char_seq<'-', str...>> {
            using value = typename parse_instr_ptr<hold<T...>, hold<acc_types..., parse_reg_or_num_v<hold<T...>, char_seq<acc_str...>>, plus>, char_seq<'-'>, char_seq<str...>>::value;
          };

          template <class ...T, typename... acc_types, char... acc_str, char f_symb, char... str>
          struct parse_instr_ptr<hold<T...>, hold<acc_types...>, char_seq<acc_str...>,
                                char_seq<f_symb, str...>> {
            using value = typename parse_instr_ptr<hold<T...>, hold<acc_types...>,char_seq<acc_str..., f_symb>, char_seq<str...>>::value;
          };

          template <class ...T, typename... acc_types, char... acc_str, char... str>
          struct parse_instr_ptr<hold<T...>, hold<acc_types...>, char_seq<acc_str...>,
                                char_seq<']', str...>> {
            using value = ptr<acc_types..., parse_reg_or_num_v<hold<T...>, char_seq<acc_str...>>>;
          };

      } // namespace
      
      template <typename... T> struct parse_instr_operand {
          using value = void;  
      };

      template <class ...T, char... str> 
      struct parse_instr_operand<hold<T...>, char_seq<'[', str...>> {
          using value =
              typename parse_instr_ptr<hold<T...>, hold<>, null_sequence<char>,
                                      char_seq<str...>>::value;
      };

      template <class ...T, typename reg_sz, char... str> 
      struct parse_instr_operand<hold<T...>, reg_sz, char_seq<'[', str...>> {
          using value =
              typename parse_instr_ptr<hold<T...>, hold<reg_sz>, null_sequence<char>,
                                      char_seq<str...>>::value;
      };

      template <class ...T, typename reg_sz, char... str>
      struct parse_instr_operand<hold<T...>, reg_sz, char_seq<'p', 't', 'r', '[', str...>> {
        using value = typename parse_instr_ptr<hold<T...>, hold<reg_sz>, null_sequence<char>,
                                              char_seq<str...>>::value;
      };

      template <class ...T,char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'b', 'y', 't', 'e', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg8, char_seq<str...>>::value;
      };

      template <class ...T,char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg16, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'d', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg32, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'q', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg64, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'o', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg128, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'y', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg256, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'z', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg512, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'t', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg80, char_seq<str...>>::value;
      };

      template <class ...T, char... str> 
      struct parse_instr_operand<hold<T...>, char_seq<str...>> {
          using value = parse_reg_or_num_v<hold<T...>, char_seq<str...>>;
      };

      template <class ...T, typename reg_sz, char... str>
      struct parse_instr_operand<hold<T...>, reg_sz, char_seq<str...>> {
          using value = parse_reg_or_num_v<hold<T...>, reg_sz, char_seq<str...>>;
      };

      template <typename... T> struct parse_instr_operands { using value = void; };

      template <class ...T, typename... acc_types, char... acc_str, char... str>
      struct parse_instr_operands<hold<T...>, hold<acc_types...>, char_seq<acc_str...>,
                                  char_seq<str...>> {
          using value = typename parse_instr_operands<
              hold<T...>, hold<acc_types...>, char_seq<acc_str..., first_elem<str...>::value>,
              typename _slice_<str...>::value>::value;
      };

      template <class ...T, typename... acc_types, char... acc_str, char... str>
      struct parse_instr_operands<hold<T...>, hold<acc_types...>, char_seq<acc_str...>,
                                  char_seq<',', str...>> {
          using value = typename parse_instr_operands<
               hold<T...>,  hold<acc_types...,
                  typename parse_instr_operand<hold<T...>, char_seq<acc_str...>>::value>,
              null_sequence<char>, char_seq<str...>>::value;
      };

      template <class ...T, typename... acc_types, char... str>
      struct parse_instr_operands<hold<T...>, hold<acc_types...>, char_seq<>,
                                  char_seq<',', str...>> {
          using value = typename parse_instr_operands<
              hold<T...>, hold<acc_types...>, char_seq<first_elem<str...>::value>,
              typename _slice_<str...>::value>::value;
      };

      template <class ...T, typename... acc_types, char... acc_str, char... str>
      struct parse_instr_operands<hold<T...>, hold<acc_types...>, char_seq<acc_str...>,
                                  char_seq<'d', 'u', 'p', '(', str...>> {
          using value = typename parse_instr_operands<
              hold<T...>, hold<acc_types..., dup<unpack_value<typename parse_instr_operand<hold<T...>, char_seq<acc_str...>>::value>::value, typename parse_instr_operands<hold<T...>, hold<>, char_seq<>, char_seq<str...>>::value>>,
              null_sequence<char>, skip_until_t<char_seq<str...>, ')'> >::value;
      };

      template <class ...T, typename... acc_types, char... acc_str, char... str>
      struct parse_instr_operands<hold<T...>, hold<acc_types...>, char_seq<acc_str...>,
                                  char_seq<';', str...>> {
          using value = hold<acc_types...,
                          typename parse_instr_operand<hold<T...>, char_seq<acc_str...>>::value>;
      };

      template <class ...T, typename... acc_types, char... acc_str, char... str>
      struct parse_instr_operands<hold<T...>, hold<acc_types...>, char_seq<acc_str...>,
                                  char_seq<')', str...>> {
          using value = hold<acc_types...,
                          typename parse_instr_operand<hold<T...>, char_seq<acc_str...>>::value>;
      };

      template <class ...T, typename... acc_types, char... str>
      struct parse_instr_operands<hold<T...>, hold<acc_types...>, char_seq<>,
                                  char_seq<';', str...>> {
        using value = hold<acc_types...>;
      };

      template <typename... T> struct parse_instr_name {
          using value = void;
      };

      template <typename... T> struct parse_instr {};

      template <class... T, char... acc_str, char... str>
      struct parse_instr<hold<T...>, char_seq<acc_str...>, char_seq<str...>> {
          using value = typename parse_instr<hold<T...>, char_seq<acc_str..., first_elem<str...>::value>,
                          typename _slice_<str...>::value>::value;
      };

      template <class... T, char... acc_str, char... str>
      struct parse_instr<hold<T...>, char_seq<acc_str...>, char_seq<' ', str...>> {
          using value = typename parse_instr_name<char_seq<acc_str...>,
            typename parse_instr_operands<
                hold<T...>, hold<>, null_sequence<char>,
                            typename skip_spaces<null_sequence<char>, char_seq<str...>>::value>::value>::value;
      };

      template <class... T, char... acc_str, char... str>
      struct parse_instr<hold<T...>, char_seq<acc_str...>, char_seq<';', str...>> {
        using value = typename parse_instr_name<char_seq<acc_str...>, hold<>>::value;
      };

      template <std::size_t N, typename, typename, typename> struct parse_asm {};

      template <std::size_t N, class ...T, char... acc_str, char... str> 
      struct parse_asm<N, hold<T...>, char_seq<acc_str...>, char_seq<';', str...>> {
        using m_v = typename parse_instr<hold<T...>, null_sequence<char>, char_seq<acc_str..., ';'>>::value;

        using value = expand_byte_seq_v<m_v,
          typename parse_asm<N + m_v{}.size(), hold<T...>, null_sequence<char>, char_seq<str...>>::value>;
      };

      template <std::size_t N, class ...T, char... acc_str, char c, char... chars> 
      struct parse_asm<N, hold<T...>, char_seq<acc_str...>, char_seq<c, chars...>>{
        using value = typename parse_asm<N, hold<T...>, char_seq<acc_str..., c>, char_seq<chars...>>::value;
      };

      template <std::size_t N, class ...T, char... acc_str,  char... chars> 
      struct parse_asm<N, hold<T...>, char_seq<acc_str...>, char_seq<':', chars...>>{
        using value = typename parse_asm<N, hold<T..., hold<char_seq<acc_str...>, ub<N>>>, char_seq<>, char_seq<chars...>>::value;
      };

      template <std::size_t N, class ...T, char... acc_str, char... str> 
      struct parse_asm<N, hold<T...>, char_seq<acc_str...>, char_seq<'\n', str...>>{
        using value = typename parse_asm<N, hold<T...>, char_seq<acc_str...>, char_seq<';', str...>>::value; 
      };

      template <std::size_t N, class ...T, char... acc_str> 
      struct parse_asm<N, hold<T...>, char_seq<acc_str...>, char_seq<>>{
        using value = typename parse_instr<hold<T...>, null_sequence<char>, char_seq<acc_str..., ';'>>::value;
      };

      template <std::size_t N, class ...T, char... acc_str, char... str> 
      struct parse_asm<N, hold<T...>, char_seq<acc_str...>, char_seq<'\0', str...>>{ 
        using value = typename parse_asm<N, hold<T...>, char_seq<acc_str...>, char_seq<>>::value;
      };

      template <std::size_t N, class ...T> 
      struct parse_asm<N, hold<T...>, char_seq<>, char_seq<>>{
        using value = null_sequence<uint8_t>;
      };
    } // namespace
} // namespace

//PoC Preprocessor
namespace{

  template <char Symbol>
  using is_letter = std::bool_constant<((Symbol >= 'A') && (Symbol <= 'Z')) ||
                                       ((Symbol >= 'a') && (Symbol <= 'z'))>;

  template <char Symbol>
  using is_digit = std::bool_constant<(Symbol >= '0') && (Symbol <= '9')>;

  template <char Symbol>
  using is_space = std::bool_constant<Symbol == ' ' || Symbol == '\t' || Symbol == '\n'>;

  template <char Symbol>
  using is_identifier_char = std::bool_constant<is_letter<Symbol>::value || is_digit<Symbol>::value || Symbol == '_'>;

  template<typename, typename>
  struct skip_word {};

  template<char ...AStr, char Felem,  char ...Str>
  struct skip_word<char_seq<AStr...>, char_seq<Felem,Str...>> {
    using type = std::conditional_t<
        is_identifier_char<Felem>::value,
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
    using type = std::bool_constant<!is_identifier_char<RFirst>::value>;
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
  
    using type = std::conditional_t<(!is_identifier_char<FChar>::value) && std::is_same<char_seq<FindStr...>, skip_word_t<char_seq<Str...>>>::value, 
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
  using parse_args = parse_columns_t<skip_spaces_t<collect_until_t<skip_until_t<char_seq<Str...>, '('>, ')'>>>;

  

  template <typename, typename, typename> struct paste_defs_impl {};


  template<typename FDef, char ...AStr, char FStr, char ...Str>
  struct paste_defs_impl<FDef, char_seq<AStr...>, char_seq<FStr, Str...>>{
    using type = std::conditional_t<
        compare_t<typename FDef::head, char_seq<FStr, Str...>>::value &&
            (parse_args<Str...>::sz == FDef::tail::tail::head::sz),
     typename paste_defs_impl<FDef, 
       char_expand_t<char_seq<AStr...>, typename fucking_smth<parse_args<Str...>, typename FDef::tail::head, 0>::type>, 
       skip_for_t<char_seq<FStr, Str...>, typename FDef::head{}.size() +  collect_until_t<char_seq<Str...>, ')'>{}.size()>>::type,
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
    using name = skip_spaces_t<collect_until_t<char_seq<Str...>, '('>>;
    using args = parse_args<Str...>;
    using def  = collect_until_t<skip_until_t<char_seq<Str...>, ')'>, ';'>;

    using processed_def = process_def_t<args, def>;

    using type = hold<name, processed_def, args>;
  };

  template <typename... T> struct parse_macro_xdef {};

  template <typename ...Defs, char... Str>
  struct parse_macro_xdef<hold<Defs...>, char_seq<Str...>> {
    using name = skip_spaces_t<collect_until_t<char_seq<Str...>, '('>>;
    using args = parse_args<Str...>;
    using def = paste_defs_t<hold<Defs...>, collect_until_t<skip_until_t<char_seq<Str...>, ')'>, ';'>>;

    using processed_def = process_def_t<args, def>;

    using type = hold<name, processed_def, args>;
  };
   
  template <typename... T> struct parse_macro_undef {};

  template <typename... Defs, char... Str>
  struct parse_macro_undef<hold<Defs...>, char_seq<Str...>> {
    using name = skip_spaces_t<collect_until_t<char_seq<Str...>, ';'>>;

    using type = find_and_delete_t<hold<>, hold<Defs...>, name>;
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
    using type = typename asm_preprocess<hold<typename parse_macro_def<char_seq<str...>>::type, Defs...>, char_seq<acc_str...>, skip_until_t<char_seq<str...>, ';'>>::type;
  };

  template<typename ...Defs, char ...acc_str, char ...str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<'%','x', 'd', 'e', 'f', 'i', 'n', 'e', ' ', str...>>{
    using type = typename asm_preprocess<hold<typename parse_macro_xdef<hold<Defs...>, char_seq<str...>>::type, Defs...>, char_seq<acc_str...>, skip_until_t<char_seq<str...>, ';'>>::type;
  };

  template<typename ...Defs, char ...acc_str, char ...str>
  struct asm_preprocess<hold<Defs...>, char_seq<acc_str...>, char_seq<'%', 'u', 'n', 'd', 'e', 'f', ' ', str...>>{
    using type = typename asm_preprocess<typename parse_macro_undef<hold<Defs...>, char_seq<str...>>::type, char_seq<acc_str...>, skip_until_t<char_seq<str...>, ';'>>::type;
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

  namespace {
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
}


template<typename>
static constexpr std::array<uint8_t, 0> parse_asm_v = {};
template<char... str>
static constexpr auto parse_asm_v<char_seq<str...>> = seq_to_arr<typename parse_asm<0, hold<>, null_sequence<char>, char_seq<str...>>::value>::value;

template <std::size_t N, const char (&s)[N], typename T>  
struct make_char_sequence_impl;

template <std::size_t N, const char (&s)[N], std::size_t... i>
struct make_char_sequence_impl<N, s, std::index_sequence<i...>> {
  using type = char_seq<s[i]...>;
};

template <std::size_t N, const char (&Input)[N]>
using make_char_sequence = typename make_char_sequence_impl<
    N, Input, std::make_index_sequence<N>>::type;

#define x86asm(str) ([](){static constexpr const char literal[] = (str); return parse_asm_v<make_char_sequence<sizeof(literal), literal>>;}())
