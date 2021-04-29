#pragma once
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
  using value = std::integer_sequence<uint8_t>;
};

template <uint8_t... a>
struct expand_byte_seq<std::integer_sequence<uint8_t, a...>> {
  using value = std::integer_sequence<uint8_t, a...>;
};

template <uint8_t... a, uint8_t... a1>
struct expand_byte_seq<std::integer_sequence<uint8_t, a...>,
                       std::integer_sequence<uint8_t, a1...>> {
  using value = std::integer_sequence<uint8_t, a..., a1...>;
};

template <uint8_t... a, uint8_t... a1, typename... T>
struct expand_byte_seq<std::integer_sequence<uint8_t, a...>,
                       std::integer_sequence<uint8_t, a1...>, T...> {
  using value =
      typename expand_byte_seq<std::integer_sequence<uint8_t, a..., a1...>,
                               T...>::value;
};

template<typename ...T>
using expand_byte_seq_v = typename expand_byte_seq<T...>::value;

template <typename>
struct seq_to_arr {};

template <uint8_t... ints>
struct seq_to_arr<std::integer_sequence<uint8_t, ints...>> {
  static constexpr auto value = std::array<uint8_t, sizeof...(ints)>{ints...};
};

template <typename... T> struct hold {};

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
template <uint8_t N> using tmm = _mm<zip<reg512>, N>;

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

template <uint8_t D> struct ub {
  using value = byte_seq<D>;
};

template <uint16_t D> struct uw {
  using value = byte_seq<(uint8_t)(D & 0xFF), (uint8_t)((D >> 8) & 0xFF)>;
};

template <uint32_t D> struct ud {
  using value = byte_seq<
      (uint8_t)(D & 0xFF), (uint8_t)((D >> 8) & 0xFF),
      (uint8_t)((D >> 16) & 0xFF), (uint8_t)((D >> 24) & 0xFF)>;
};

template <uint64_t D> struct uq {
  using value = byte_seq<
      (uint8_t)(D & 0xFF),         (uint8_t)((D >> 8) & 0xFF),
      (uint8_t)((D >> 16) & 0xFF), (uint8_t)((D >> 24) & 0xFF),
      (uint8_t)((D >> 32) & 0xFF), (uint8_t)((D >> 40) & 0xFF),
      (uint8_t)((D >> 48) & 0xFF), (uint8_t)((D >> 56) & 0xFF)>;
};

template <int8_t N>  struct ib : ub<static_cast<uint8_t>(N)> {};
template <int16_t N> struct iw : uw<static_cast<uint16_t>(N)> {};
template <int32_t N> struct id : ud<static_cast<uint32_t>(N)> {};
template <int64_t N> struct iq : uq<static_cast<uint64_t>(N)> {};

template <uint8_t N>  using disp8  = ub<N>;
template <uint16_t N> using disp16 = uw<N>;
template <uint32_t N> using disp32 = ud<N>;
template <uint64_t N> using disp64 = uq<N>;

template <uint8_t N>  using rel8  = ub<N>;
template <uint16_t N> using rel16 = uw<N>;
template <uint32_t N> using rel32 = ud<N>;
template <uint64_t N> using rel64 = uq<N>;

template <typename... T> struct disp_reg {};

template <typename... T, uint8_t N>  struct disp_reg<hold<T...>, ub<N>> : ub<N> {};
template <typename... T, uint16_t N> struct disp_reg<hold<T...>, uw<N>> : uw<N> {};
template <typename... T, uint32_t N> struct disp_reg<hold<T...>, ud<N>> : ud<N> {};
template <typename... T, uint64_t N> struct disp_reg<hold<T...>, uq<N>> : uq<N> {};

template<typename Ty>
struct u64_8{};

template <uint8_t V>
struct u64_8<ub<V>> {
  using type = bool;
  using value = typename uq<V>::value;
};

template <uint16_t V>
struct u64_8<uw<V>> {
  using type = bool;
  using value = typename uq<V>::value;
};

template <uint32_t V>
struct u64_8<ud<V>> {
  using type = bool;
  using value = typename uq<V>::value;
};

template <uint64_t V>
struct u64_8<uq<V>> {
  using type = bool;
  using value = typename uq<V>::value;
};

template<typename Ty>
struct u32_8{};

template <uint8_t V>
struct u32_8<ub<V>> {
  using type = bool;
  using value = typename ud<V>::value;
};

template <uint16_t V>
struct u32_8<uw<V>> {
  using type = bool;
  using value = typename ud<V>::value;  
};

template <uint32_t V>
struct u32_8<ud<V>> {
  using type = bool;
  using value = typename ud<V>::value;
};

template<typename Ty>
struct u16_8{};

template <uint8_t V>
struct u16_8<ub<V>> {
  using type = bool;
  using value = typename uw<V>::value;
};

template <uint16_t V>
struct u16_8<uw<V>> {
  using type = bool;
  using value = typename uw<V>::value;
};

template<typename Ty>
struct u8_8{};

template <uint8_t V>
struct u8_8<ub<V>> {
  using type = bool;
  using value = typename ub<V>::value;
};

template<typename Ty>
struct i64_8{};

template <uint8_t V>
struct i64_8<ib<V>> {
  using type = bool;
  using value = typename iq<V>::value;
};

template <uint16_t V>
struct i64_8<iw<V>> {
  using type = bool;
  using value = typename iq<V>::value;
};

template <uint32_t V>
struct i64_8<id<V>> {
  using type = bool;
  using value = typename iq<V>::value;
};

template <uint64_t V>
struct i64_8<iq<V>> {
  using type = bool;
  using value = typename iq<V>::value;
};

template<typename Ty>
struct i32_8{};

template <uint8_t V>
struct i32_8<ib<V>> {
  using type = bool;
  using value = typename id<V>::value;
};

template <uint16_t V>
struct i32_8<iw<V>> {
  using type = bool;
  using value = typename id<V>::value;  
};

template <uint32_t V>
struct i32_8<id<V>> {
  using type = bool;
  using value = typename id<V>::value;
};

template<typename Ty>
struct i16_8{};

template <uint8_t V>
struct i16_8<ib<V>> {
  using type = bool;
  using value = typename iw<V>::value;
};

template <uint16_t V>
struct i16_8<iw<V>> {
  using type = bool;
  using value = typename iw<V>::value;
};

template<typename Ty>
struct i8_8{};

template <uint8_t V>
struct i8_8<ib<V>> {
  using type = bool;
  using value = typename ib<V>::value;
};

template<typename  > struct unpack_value;
template<uint8_t  D> struct unpack_value<ub<D>>: std::integral_constant<std::size_t, D>{};
template<uint16_t D> struct unpack_value<uw<D>>: std::integral_constant<std::size_t, D>{};
template<uint32_t D> struct unpack_value<ud<D>>: std::integral_constant<std::size_t, D>{};
template<uint64_t D> struct unpack_value<uq<D>>: std::integral_constant<std::size_t, D>{};

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
struct dup<1, T>{
  using value = typename unpack_hold<T>::value;
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
      reg_n<hold<T...>>::value | (is_ext_v<hold<T...>> << 3);

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

  static constexpr auto _67h = 0;
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
        typename std::conditional_t<std::is_same_v<T, reg32>, std::is_same<regs_bit, reg64>,
        typename std::conditional_t<std::is_same_v<T, reg16>, std::is_same<regs_bit, reg32>,
        typename std::conditional_t<std::is_same_v<T, reg8>,  std::is_same<regs_bit, reg16>,
                       std::false_type>>>::type;

template <class Y>
struct mrm<Y> {
    template <typename T>
    using value = typename
      modrm<ptr<extract_args_v<typename Y::type>>, T>::value;

    static constexpr auto R = 0;
    static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
    static constexpr auto B = 0;

    static constexpr auto _67h = std::is_same_v<is_67h_needed<extract_head_v<typename Y::type>>,
        std::true_type>;
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

    static constexpr auto _67h = std::is_same_v<is_67h_needed<extract_head_v<typename Y::type>>,
                       std::true_type>;
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

    static constexpr auto _67h = std::is_same_v<is_67h_needed<extract_head_v<typename Y::type>>,
                       std::true_type>;
};

template <class Y, uint8_t Scale, class Z>
struct mrm<Y, mul, disp8<Scale>, plus, Z> {
    template <typename T>
    using value = typename
      modrm<SIB<disp8<Scale>, extract_args_v<typename Y::type>, extract_args_v<typename Z::type>>, T>::value;

    static constexpr auto R = 0;
    static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
    static constexpr auto B = is_ext_v<extract_args_v<typename Z::type>>;

    static constexpr auto _67h = std::is_same_v<is_67h_needed<extract_head_v<typename Y::type>>,
                       std::true_type>;
};

template <class Y, uint8_t Scale, class Z, uint8_t D>
struct mrm<Y, mul, disp8<Scale>, plus, Z, plus, disp8<D>> {
    template <typename T>
    using value = typename
        modrm<disp_SIB<disp8<Scale>, disp8<D>, extract_args_v<typename Y::type>, extract_args_v<typename Z::type>>, T>::value;

    static constexpr auto R = 0;
    static constexpr auto X = is_ext_v<extract_args_v<typename Y::type>>;
    static constexpr auto B = is_ext_v<extract_args_v<typename Z::type>>;

    static constexpr auto _67h = std::is_same_v<is_67h_needed<extract_head_v<typename Y::type>>,
                       std::true_type>;
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

  static constexpr auto _67h =
      std::is_same_v<is_67h_needed<extract_head_v<typename Y::type>>,
                     std::true_type>;
};

template<bool n>
struct byte_67h {
  using value = std::integer_sequence<uint8_t>;
};

template <>
struct byte_67h<true> {
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

    template <std::size_t N> struct num {};

    template <typename... T> struct get_elem { static constexpr char value = 0; };
    template <char f, char... chars> struct get_elem<char_seq<f, chars...>> {
      static constexpr char value = f;
    };

  namespace {
      template <char a, char... b> struct _slice_ {
        using value = std::integer_sequence<char, b...>;
      };
      template <typename... T> struct _first_elements {};

      template <std::size_t end, char... chars>
      struct _first_elements<num<end>, std::integer_sequence<char, chars...>> {
        using value =
            typename _first_elements<num<end - 1>,
                                    typename _slice_<chars...>::value>::value;
      };

      template <char... chars>
      struct _first_elements<num<0>, std::integer_sequence<char, chars...>> {
        using value = typename std::integer_sequence<char, chars...>;
      };

      template <std::size_t end, char... chars>
      struct slice
          : _first_elements<num<end>, std::integer_sequence<char, chars...>> {};

  } // namespace

  namespace {
      namespace {

          template <uint64_t x, uint64_t y>
          struct _pow{
            static constexpr uint64_t value = _pow<x * x, y - 1>::value;
          };

          template <uint64_t x>
          struct _pow<x, 1>{
            static constexpr uint64_t value = x;
          };

          template<typename, char> struct skip_until{};
          template<char el, char... str, char c>
          struct skip_until<char_seq<el, str...>, c>{
            using value = typename skip_until<char_seq<str...>, c>::value;
          };

          template<char... str, char c>
          struct skip_until<char_seq<c, str...>, c>{
            using value = char_seq<str...>;
          };

          template<typename T, char c>
          using skip_until_v = typename skip_until<T, c>::value;


          template <typename T, typename Y> struct skip_underscores {};
          template <char... acc_str, char a, char... str>
          struct skip_underscores<char_seq<acc_str...>, char_seq<a, str...>> {
              using value =
                  typename skip_underscores<char_seq<acc_str..., a>, char_seq<str...>>::value;
          };
          template <char... acc_str, char... str>
          struct skip_underscores<char_seq<acc_str...>, char_seq<'_', str...>> {
              using value =
                  typename skip_underscores<char_seq<acc_str...>, char_seq<str...>>::value;
          };
          template <char... acc_str>
          struct skip_underscores<char_seq<acc_str...>, null_sequence<char>> {
              using value = char_seq<acc_str...>;
          };

          template <typename T, uint64_t N>
          struct parse_num_impl {
            static constexpr uint64_t value = -1;
          };

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
          struct make_null_seq<1> {
            using value = std::integer_sequence<uint8_t, 0x00>;
          };

          template <>
          struct make_null_seq<0> {
            using value = std::integer_sequence<uint8_t>;
          };

          template<typename>
          struct parse_str {};

          template <char c>
          struct parse_str<char_seq<c, '\''>> {
            using value = ub<static_cast<uint8_t>(c)>;
          };

          template <char c, char c1>
          struct parse_str<char_seq<c, c1, '\''>> {
            using value = uw<static_cast<uint16_t>((c << 8) +  c1)>;
          };

          template <char c, char c1, char c2>
          struct parse_str<char_seq<c, c1, c2, '\''>> {
            using value = ud<static_cast<uint32_t>((c << 24) + (c1 << 16) + (c2 << 8) + 0)>;
          };

          template <char c, char c1, char c2, char c3>
          struct parse_str<char_seq<c, c1, c2, c3, '\''>> {
            using value = ud<static_cast<uint32_t>((c << 24) + (c1 << 16) +
                                                   (c2 << 8) + c3)>;
          };

          template <char c, char c1, char c2, char c3, char c4, char c5, char c6, char c7>
          struct parse_str<char_seq<c, c1, c2, c3, c4, c5, c6, c7, '\''>> {
            using value = uq<
            static_cast<uint64_t>(
              (uint64_t(c) << 56) + (uint64_t(c1) << 48) + (uint64_t(c2) << 40) + (uint64_t(c3) << 32) +
              (uint64_t(c4) << 24) + (uint64_t(c5) << 16) + (uint64_t(c6) << 8) + uint64_t(c7))>;
          };

          /*template<typename, typename, uint8_t>
          struct parse_str_additional{};

          template<char... chars, typename ...T, uint8_t align_to>
          struct parse_str_additional<hold<T...>, char_seq<chars...>, align_to>{
            using value = expand_byte_seq_v<byte_seq<chars...>, make_null_seq<8 - (sizeof...(chars) % 8)>>;
          };*/

          /*
          template<char... chars>
          struct parse_str<char_seq<chars...>> {
            using value = expand_byte_seq_v<byte_seq<chars...>, make_null_seq<8 - (sizeof...(chars) % 8)>>;
          };*/

          template<typename T>
          using parse_str_v = typename parse_str<T>::value;

          template <typename T> struct parse_instr_register {
            using value = void;
          };

          template <> struct parse_instr_register<char_seq<'a', 'l'>> {
            using value = al;
          };
          template <> struct parse_instr_register<char_seq<'c', 'l'>> {
            using value = cl;
          };
          template <> struct parse_instr_register<char_seq<'d', 'l'>> {
            using value = dl;
          };
          template <> struct parse_instr_register<char_seq<'b', 'l'>> {
            using value = bl;
          };
          template <> struct parse_instr_register<char_seq<'a', 'h'>> {
            using value = ah;
          };
          template <> struct parse_instr_register<char_seq<'c', 'h'>> {
            using value = ch;
          };
          template <> struct parse_instr_register<char_seq<'d', 'h'>> {
            using value = dh;
          };
          template <> struct parse_instr_register<char_seq<'b', 'h'>> {
            using value = bh;
          };
          
          template <> struct parse_instr_register<char_seq<'r', '8', 'l'>> {
            using value = r8l;
          };
          template <> struct parse_instr_register<char_seq<'r', '9', 'l'>> {
            using value = r9l;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '0', 'l'>> {
            using value = r10l;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '1', 'l'>> {
            using value = r11l;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '2', 'l'>> {
            using value = r12l;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '3', 'l'>> {
            using value = r13l;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '4', 'l'>> {
            using value = r14l;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '5', 'l'>> {
            using value = r15l;
          };

          template <> struct parse_instr_register<char_seq<'a', 'x'>> {
            using value = ax;
          };
          template <> struct parse_instr_register<char_seq<'b', 'x'>> {
            using value = bx;
          };
          template <> struct parse_instr_register<char_seq<'c', 'x'>> {
            using value = cx;
          };
          template <> struct parse_instr_register<char_seq<'d', 'x'>> {
            using value = dx;
          };
          template <> struct parse_instr_register<char_seq<'s', 'i'>> {
            using value = si;
          };
          template <> struct parse_instr_register<char_seq<'d', 'i'>> {
            using value = di;
          };
          template <> struct parse_instr_register<char_seq<'s', 'p'>> {
            using value = sp;
          };
          template <> struct parse_instr_register<char_seq<'b', 'p'>> {
            using value = bp;
          };

          template <> struct parse_instr_register<char_seq<'r', '8', 'w'>> {
            using value = r8w;
          };
          template <> struct parse_instr_register<char_seq<'r', '9', 'w'>> {
            using value = r9w;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '0', 'w'>> {
            using value = r10w;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '1', 'w'>> {
            using value = r11w;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '2', 'w'>> {
            using value = r12w;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '3', 'w'>> {
            using value = r13w;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '4', 'w'>> {
            using value = r14w;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '5', 'w'>> {
            using value = r15w;
          };

          template <> struct parse_instr_register<char_seq<'e', 'a', 'x'>> {
            using value = eax;
          };
          template <> struct parse_instr_register<char_seq<'e', 'b', 'x'>> {
            using value = ebx;
          };
          template <> struct parse_instr_register<char_seq<'e', 'c', 'x'>> {
            using value = ecx;
          };
          template <> struct parse_instr_register<char_seq<'e', 'd', 'x'>> {
            using value = edx;
          };
          template <> struct parse_instr_register<char_seq<'e', 's', 'i'>> {
            using value = esi;
          };
          template <> struct parse_instr_register<char_seq<'e', 'd', 'i'>> {
            using value = edi;
          };
          template <> struct parse_instr_register<char_seq<'e', 's', 'p'>> {
            using value = esp;
          };
          template <> struct parse_instr_register<char_seq<'e', 'b', 'p'>> {
            using value = ebp;
          };

          template <> struct parse_instr_register<char_seq<'z', 'a', 'x'>> {
            using value = zax;
          };
          template <> struct parse_instr_register<char_seq<'z', 'b', 'x'>> {
            using value = zbx;
          };
          template <> struct parse_instr_register<char_seq<'z', 'c', 'x'>> {
            using value = zcx;
          };
          template <> struct parse_instr_register<char_seq<'z', 'd', 'x'>> {
            using value = zdx;
          };
          template <> struct parse_instr_register<char_seq<'z', 's', 'i'>> {
            using value = zsi;
          };
          template <> struct parse_instr_register<char_seq<'z', 'd', 'i'>> {
            using value = zdi;
          };
          template <> struct parse_instr_register<char_seq<'z', 's', 'p'>> {
            using value = zsp;
          };
          template <> struct parse_instr_register<char_seq<'z', 'b', 'p'>> {
            using value = zbp;
          };

          template <> struct parse_instr_register<char_seq<'r', '8', 'd'>> {
            using value = r8d;
          };
          template <> struct parse_instr_register<char_seq<'r', '9', 'd'>> {
            using value = r9d;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '0', 'd'>> {
            using value = r10d;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '1', 'd'>> {
            using value = r11d;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '2', 'd'>> {
            using value = r12d;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '3', 'd'>> {
            using value = r13d;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '4', 'd'>> {
            using value = r14d;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '5', 'd'>> {
            using value = r15d;
          };

          template <> struct parse_instr_register<char_seq<'r', 'a', 'x'>> {
            using value = rax;
          };
          template <> struct parse_instr_register<char_seq<'r', 'b', 'x'>> {
            using value = rbx;
          };
          template <> struct parse_instr_register<char_seq<'r', 'c', 'x'>> {
            using value = rcx;
          };
          template <> struct parse_instr_register<char_seq<'r', 'd', 'x'>> {
            using value = rdx;
          };
          template <> struct parse_instr_register<char_seq<'r', 's', 'i'>> {
            using value = rsi;
          };
          template <> struct parse_instr_register<char_seq<'r', 'd', 'i'>> {
            using value = rdi;
          };
          template <> struct parse_instr_register<char_seq<'r', 's', 'p'>> {
            using value = rsp;
          };
          template <> struct parse_instr_register<char_seq<'r', 'b', 'p'>> {
            using value = rbp;
          };

          template <> struct parse_instr_register<char_seq<'r', '8'>> {
            using value = r8;
          };
          template <> struct parse_instr_register<char_seq<'r', '9'>> {
            using value = r9;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '0'>> {
            using value = r10;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '1'>> {
            using value = r11;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '2'>> {
            using value = r12;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '3'>> {
            using value = r13;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '4'>> {
            using value = r14;
          };
          template <> struct parse_instr_register<char_seq<'r', '1', '5'>> {
            using value = r15;
          };

          template <char... chars>
          struct parse_instr_register<char_seq<'s', 't', chars...>> {
            using value = st<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };

          template <char... chars>
          struct parse_instr_register<char_seq<'m', 'm', chars...>> {
            using value = mm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };

          template <char... chars>
          struct parse_instr_register<char_seq<'x', 'm', 'm', chars...>> {
            using value = xmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };
          template <char... chars>
          struct parse_instr_register<char_seq<'y', 'm', 'm', chars...>> {
            using value = ymm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };
          template <char... chars>
          struct parse_instr_register<char_seq<'z', 'm', 'm', chars...>> {
            using value = zmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };

          template <char... chars>
          struct parse_instr_register<char_seq<'t', 'm', 'm', chars...>> {
            using value = tmm<static_cast<uint8_t>(parse_num<char_seq<chars...>>::value)>;
          };

          template<typename...>
          struct parse_reg_or_num {};

          template <class ...T, typename reg_sz, char... chars>
          struct parse_reg_or_num<hold<T...>, reg_sz, char_seq<chars...>>{
              using value = 
                  typename std::conditional_t<std::is_same_v<reg_sz, reg8>,  ub< uint8_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same_v<reg_sz, reg16>, uw<uint16_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same_v<reg_sz, reg32>, ud<uint32_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same_v<reg_sz, reg64>, uq<uint64_t(parse_num<char_seq<chars...>>::value)>, void
                  >>>>;
          };

          template <class ...T, typename reg_sz, char... chars>
          struct parse_reg_or_num<hold<T...>, reg_sz, char_seq<'-', chars...>>{
              using value = 
                  typename std::conditional_t<std::is_same_v<reg_sz, reg8>,  ib< -int8_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same_v<reg_sz, reg16>, iw<-int16_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same_v<reg_sz, reg32>, id<-int32_t(parse_num<char_seq<chars...>>::value)>, 
                  typename std::conditional_t<std::is_same_v<reg_sz, reg64>, iq<-int64_t(parse_num<char_seq<chars...>>::value)>, void
                  >>>>;
          };

          /*template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<chars...>>{
              using value =  
                  typename std::conditional_t<!(first_elem<chars...>::value <= '9' && 
                                                first_elem<chars...>::value >= '0'),
                                          typename parse_instr_register<char_seq<chars...>>::value,
                                          ub<uint8_t(parse_num<char_seq<chars...>>::value)>>;
          };*/

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
                fucking_struct_impl<T, char_seq<str...>>::value,typename fucking_unpack<T>::value, void>;
          };

          template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<chars...>>{
              using value =  
                  typename std::conditional_t<!(first_elem<chars...>::value <= '9' && 
                                                first_elem<chars...>::value >= '0'),
                  typename std::conditional_t<std::is_same_v<typename parse_instr_register<char_seq<chars...>>::value, void>, typename fucking_struct<hold<T...>, char_seq<chars...>>::value , typename parse_instr_register<char_seq<chars...>>::value>,
                                          ub<uint8_t(parse_num<char_seq<chars...>>::value)>>;
          };

          template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<'-', chars...>>{
              using value =  
                  typename std::conditional_t<!(first_elem<chars...>::value <= '9' && 
                                                first_elem<chars...>::value >= '0'),
                                          typename parse_instr_register<char_seq<chars...>>::value,
                                          ub<static_cast<uint8_t>(-int8_t(parse_num<char_seq<chars...>>::value))>>;
          };

          template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<'?', chars...>> {
            using value = ub<0x00>;
          };

          template <class ...T, char... chars>
          struct parse_reg_or_num<hold<T...>, char_seq<'\'', chars...>> {
            using value = typename parse_str<char_seq<chars...>>::value;
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

      namespace {
          template <typename T, typename Y> struct skip_spaces {};
          template <char... acc_str, char a, char... str>
          struct skip_spaces<char_seq<acc_str...>, char_seq<a, str...>> {
              using value = typename skip_spaces<char_seq<acc_str..., a>,
                                               char_seq<str...>>::value;
          };
          template <char... acc_str, char... str>
          struct skip_spaces<char_seq<acc_str...>, char_seq<' ', str...>> {
              using value = typename skip_spaces<char_seq<acc_str...>,
                                               char_seq<str...>>::value;
          };
          template <char... acc_str>
          struct skip_spaces<char_seq<acc_str...>, null_sequence<char>> {
              using value = char_seq<acc_str...>;
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
      struct parse_instr_operand<hold<T...>, char_seq<'x', 'm', 'm', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg128, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'y', 'm', 'm', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg256, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'z', 'm', 'm', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg512, char_seq<str...>>::value;
      };

      template <class ...T, char... str>
      struct parse_instr_operand<hold<T...>, char_seq<'t', 'm', 'm', 'w', 'o', 'r', 'd', str...>> {
        using value = typename parse_instr_operand<hold<T...>, reg512, char_seq<str...>>::value;
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
              null_sequence<char>, skip_until_v<char_seq<str...>, ')'> >::value;
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
          typename parse_asm<N + m_v{}.size(),hold<T...>, null_sequence<char>, char_seq<str...>>::value>;
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



      template<typename>
      static constexpr std::array<uint8_t, 0> parse_asm_v = {};
      template<char... str>
      static constexpr auto parse_asm_v<char_seq<str...>> = seq_to_arr<typename parse_asm<0, hold<>, null_sequence<char>, char_seq<str...>>::value>::value;
    } // namespace
} // namespace

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
    struct parse_instr_name<char_seq<'d','z', str...>, hold<T...>> {
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
