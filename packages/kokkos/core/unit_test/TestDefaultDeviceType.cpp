/*
//@HEADER
// ************************************************************************
//
//   Kokkos: Manycore Performance-Portable Multidimensional Arrays
//              Copyright (2012) Sandia Corporation
//
// Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
// the U.S. Government retains certain rights in this software.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// 1. Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
//
// 3. Neither the name of the Corporation nor the names of the
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA CORPORATION OR THE
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// Questions? Contact  H. Carter Edwards (hcedwar@sandia.gov)
//
// ************************************************************************
//@HEADER
*/

#include <gtest/gtest.h>

#include <Kokkos_Core.hpp>

#if !defined(KOKKOS_HAVE_CUDA) || defined(__CUDACC__)
//----------------------------------------------------------------------------

#include <TestViewImpl.hpp>
#include <TestAtomic.hpp>

#include <TestMemoryTracking.hpp>
#include <TestViewAPI.hpp>

#include <TestRequest.hpp>
#include <TestReduce.hpp>
#include <TestScan.hpp>
#include <TestMultiReduce.hpp>
#include <TestAggregate.hpp>
#include <TestCompilerMacros.hpp>
#include <TestCXX11.hpp>

namespace Test {

class defaultdevicetype : public ::testing::Test {
protected:
  static void SetUpTestCase()
  {
    Kokkos::initialize();
  }

  static void TearDownTestCase()
  {
    Kokkos::finalize();
  }
};


TEST_F( defaultdevicetype, view_impl) {
  test_view_impl< Kokkos::Impl::DefaultDeviceType >();
}

TEST_F( defaultdevicetype, view_api) {
  TestViewAPI< double , Kokkos::Impl::DefaultDeviceType >();
}

TEST_F( defaultdevicetype, long_reduce) {
  TestReduce< long ,   Kokkos::Impl::DefaultDeviceType >( 1000000 );
}

TEST_F( defaultdevicetype, double_reduce) {
  TestReduce< double ,   Kokkos::Impl::DefaultDeviceType >( 1000000 );
}

TEST_F( defaultdevicetype, long_reduce_dynamic ) {
  TestReduceDynamic< long ,   Kokkos::Impl::DefaultDeviceType >( 1000000 );
}

TEST_F( defaultdevicetype, double_reduce_dynamic ) {
  TestReduceDynamic< double ,   Kokkos::Impl::DefaultDeviceType >( 1000000 );
}

TEST_F( defaultdevicetype, long_reduce_dynamic_view ) {
  TestReduceDynamicView< long ,   Kokkos::Impl::DefaultDeviceType >( 1000000 );
}

TEST_F( defaultdevicetype, dev_long_reduce) {
  TestReduceRequest< long ,   Kokkos::Impl::DefaultDeviceType >( 1000000 );
}

TEST_F( defaultdevicetype, dev_double_reduce) {
  TestReduceRequest< double ,   Kokkos::Impl::DefaultDeviceType >( 1000000 );
}

TEST_F( defaultdevicetype, dev_shared_request) {
  TestSharedRequest< Kokkos::Impl::DefaultDeviceType >();
}


TEST_F( defaultdevicetype , atomics )
{
  const int loop_count = 1e4 ;

  ASSERT_TRUE( ( TestAtomic::Loop<int,Kokkos::Impl::DefaultDeviceType>(loop_count,1) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<int,Kokkos::Impl::DefaultDeviceType>(loop_count,2) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<int,Kokkos::Impl::DefaultDeviceType>(loop_count,3) ) );

  ASSERT_TRUE( ( TestAtomic::Loop<unsigned int,Kokkos::Impl::DefaultDeviceType>(loop_count,1) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<unsigned int,Kokkos::Impl::DefaultDeviceType>(loop_count,2) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<unsigned int,Kokkos::Impl::DefaultDeviceType>(loop_count,3) ) );

  ASSERT_TRUE( ( TestAtomic::Loop<long int,Kokkos::Impl::DefaultDeviceType>(loop_count,1) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<long int,Kokkos::Impl::DefaultDeviceType>(loop_count,2) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<long int,Kokkos::Impl::DefaultDeviceType>(loop_count,3) ) );

  ASSERT_TRUE( ( TestAtomic::Loop<unsigned long int,Kokkos::Impl::DefaultDeviceType>(loop_count,1) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<unsigned long int,Kokkos::Impl::DefaultDeviceType>(loop_count,2) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<unsigned long int,Kokkos::Impl::DefaultDeviceType>(loop_count,3) ) );

  ASSERT_TRUE( ( TestAtomic::Loop<long long int,Kokkos::Impl::DefaultDeviceType>(loop_count,1) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<long long int,Kokkos::Impl::DefaultDeviceType>(loop_count,2) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<long long int,Kokkos::Impl::DefaultDeviceType>(loop_count,3) ) );

  ASSERT_TRUE( ( TestAtomic::Loop<double,Kokkos::Impl::DefaultDeviceType>(loop_count,1) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<double,Kokkos::Impl::DefaultDeviceType>(loop_count,2) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<double,Kokkos::Impl::DefaultDeviceType>(loop_count,3) ) );

  ASSERT_TRUE( ( TestAtomic::Loop<float,Kokkos::Impl::DefaultDeviceType>(100,1) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<float,Kokkos::Impl::DefaultDeviceType>(100,2) ) );
  ASSERT_TRUE( ( TestAtomic::Loop<float,Kokkos::Impl::DefaultDeviceType>(100,3) ) );
}

/*TEST_F( defaultdevicetype , view_remap )
{
  enum { N0 = 3 , N1 = 2 , N2 = 8 , N3 = 9 };

  typedef Kokkos::View< double*[N1][N2][N3] ,
                             Kokkos::LayoutRight ,
                             Kokkos::Impl::DefaultDeviceType > output_type ;

  typedef Kokkos::View< int**[N2][N3] ,
                             Kokkos::LayoutLeft ,
                             Kokkos::Impl::DefaultDeviceType > input_type ;

  typedef Kokkos::View< int*[N0][N2][N3] ,
                             Kokkos::LayoutLeft ,
                             Kokkos::Impl::DefaultDeviceType > diff_type ;

  output_type output( "output" , N0 );
  input_type  input ( "input" , N0 , N1 );
  diff_type   diff  ( "diff" , N0 );

  int value = 0 ;
  for ( size_t i3 = 0 ; i3 < N3 ; ++i3 ) {
  for ( size_t i2 = 0 ; i2 < N2 ; ++i2 ) {
  for ( size_t i1 = 0 ; i1 < N1 ; ++i1 ) {
  for ( size_t i0 = 0 ; i0 < N0 ; ++i0 ) {
    input(i0,i1,i2,i3) = ++value ;
  }}}}

  // Kokkos::deep_copy( diff , input ); // throw with incompatible shape
  Kokkos::deep_copy( output , input );

  value = 0 ;
  for ( size_t i3 = 0 ; i3 < N3 ; ++i3 ) {
  for ( size_t i2 = 0 ; i2 < N2 ; ++i2 ) {
  for ( size_t i1 = 0 ; i1 < N1 ; ++i1 ) {
  for ( size_t i0 = 0 ; i0 < N0 ; ++i0 ) {
    ++value ;
    ASSERT_EQ( value , ((int) output(i0,i1,i2,i3) ) );
  }}}}
}*/

//----------------------------------------------------------------------------


TEST_F( defaultdevicetype , view_aggregate )
{
  TestViewAggregate< Kokkos::Impl::DefaultDeviceType >();
}

//----------------------------------------------------------------------------

TEST_F( defaultdevicetype , scan )
{
  for ( int i = 0 ; i < 1000 ; ++i ) {
    TestScan< Kokkos::Impl::DefaultDeviceType >( 10 );
    TestScan< Kokkos::Impl::DefaultDeviceType >( 10000 );
  }
  TestScan< Kokkos::Impl::DefaultDeviceType >( 1000000 );
  TestScan< Kokkos::Impl::DefaultDeviceType >( 10000000 );
  Kokkos::Impl::DefaultDeviceType::fence();
}


TEST_F( defaultdevicetype , team_scan )
{
  TestScanRequest< Kokkos::Impl::DefaultDeviceType >( 10 );
  TestScanRequest< Kokkos::Impl::DefaultDeviceType >( 10000 );
}

//----------------------------------------------------------------------------

TEST_F( defaultdevicetype , compiler_macros )
{
  ASSERT_TRUE( ( TestCompilerMacros::Test< Kokkos::Impl::DefaultDeviceType >() ) );
}


//----------------------------------------------------------------------------
#if defined (KOKKOS_HAVE_CXX11)
TEST_F( defaultdevicetype , cxx11 )
{
  ASSERT_TRUE( ( TestCXX11::Test< Kokkos::Impl::DefaultDeviceType >(1) ) );
  ASSERT_TRUE( ( TestCXX11::Test< Kokkos::Impl::DefaultDeviceType >(2) ) );
  ASSERT_TRUE( ( TestCXX11::Test< Kokkos::Impl::DefaultDeviceType >(3) ) );
  ASSERT_TRUE( ( TestCXX11::Test< Kokkos::Impl::DefaultDeviceType >(4) ) );
}
#endif
} // namespace test

#endif