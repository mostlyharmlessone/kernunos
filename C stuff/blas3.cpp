/* =========================================================================
   Copyright (c) 2010-2016, Institute for Microelectronics,
                            Institute for Analysis and Scientific Computing,
                            TU Wien.
   Portions of this software are copyright by UChicago Argonne, LLC.
                            -----------------
                  ViennaCL - The Vienna Computing Library
                            -----------------
   Project Head:    Karl Rupp                   rupp@iue.tuwien.ac.at
   (A list of authors and contributors can be found in the PDF manual)
   License:         MIT (X11), see file LICENSE in the base directory
============================================================================= */
//disable debug mechanisms to have a fair comparison with ublas:
#ifndef NDEBUG
 #define NDEBUG
#endif
// System headers
#include <iostream>
// ublas headers
#include <boost/numeric/ublas/io.hpp>
#include <boost/numeric/ublas/triangular.hpp>
#include <boost/numeric/ublas/matrix_sparse.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/matrix_proxy.hpp>
#include <boost/numeric/ublas/lu.hpp>
#include <boost/numeric/ublas/io.hpp>
// Must be set if you want to use ViennaCL algorithms on ublas objects
#define VIENNACL_WITH_UBLAS 1
// ViennaCL headers
#include "viennacl/scalar.hpp"
#include "viennacl/vector.hpp"
#include "viennacl/matrix.hpp"
#include "viennacl/linalg/prod.hpp"
#include "viennacl/tools/random.hpp"
#include "viennacl/tools/timer.hpp"
#define BLAS3_MATRIX_SIZE   400
using namespace boost::numeric;
#ifndef VIENNACL_WITH_OPENCL
  struct dummy
  {
    std::size_t size() const { return 1; }
  };
#endif
int main()
{
  typedef float     ScalarType;
  viennacl::tools::timer timer;
  double exec_time;
  viennacl::tools::uniform_random_numbers<ScalarType> randomNumber;
  ublas::matrix<ScalarType> ublas_A(BLAS3_MATRIX_SIZE, BLAS3_MATRIX_SIZE);
  ublas::matrix<ScalarType, ublas::column_major> ublas_B(BLAS3_MATRIX_SIZE, BLAS3_MATRIX_SIZE);
  ublas::matrix<ScalarType> ublas_C(BLAS3_MATRIX_SIZE, BLAS3_MATRIX_SIZE);
  ublas::matrix<ScalarType> ublas_C1(BLAS3_MATRIX_SIZE, BLAS3_MATRIX_SIZE);
  for (unsigned int i = 0; i < ublas_A.size1(); ++i)
    for (unsigned int j = 0; j < ublas_A.size2(); ++j)
      ublas_A(i,j) = randomNumber();
  for (unsigned int i = 0; i < ublas_B.size1(); ++i)
    for (unsigned int j = 0; j < ublas_B.size2(); ++j)
      ublas_B(i,j) = randomNumber();
  //viennacl::ocl::set_context_device_type(0, viennacl::ocl::gpu_tag());  //uncomment this is you wish to use GPUs only
  viennacl::matrix<ScalarType> vcl_A(BLAS3_MATRIX_SIZE, BLAS3_MATRIX_SIZE);
  viennacl::matrix<ScalarType, viennacl::column_major> vcl_B(BLAS3_MATRIX_SIZE, BLAS3_MATRIX_SIZE);
  viennacl::matrix<ScalarType> vcl_C(BLAS3_MATRIX_SIZE, BLAS3_MATRIX_SIZE);
  std::cout << "--- Computing matrix-matrix product using ublas ---" << std::endl;
  timer.start();
  ublas_C = ublas::prod(ublas_A, ublas_B);
  exec_time = timer.get();
  std::cout << " - Execution time: " << exec_time << std::endl;
  std::cout << std::endl << "--- Computing matrix-matrix product on each available compute device using ViennaCL ---" << std::endl;
#ifdef VIENNACL_WITH_OPENCL
  std::vector<viennacl::ocl::device> devices = viennacl::ocl::current_context().devices();
#else
  dummy devices;
#endif
  for (std::size_t device_id=0; device_id<devices.size(); ++device_id)
  {
#ifdef VIENNACL_WITH_OPENCL
    viennacl::ocl::current_context().switch_device(devices[device_id]);
    std::cout << " - Device Name: " << viennacl::ocl::current_device().name() << std::endl;
#endif
    viennacl::copy(ublas_A, vcl_A);
    viennacl::copy(ublas_B, vcl_B);
    vcl_C = viennacl::linalg::prod(vcl_A, vcl_B);
    viennacl::backend::finish();
    timer.start();
    vcl_C = viennacl::linalg::prod(vcl_A, vcl_B);
    viennacl::backend::finish();
    exec_time = timer.get();
    std::cout << " - Execution time on device (no setup time included): " << exec_time << std::endl;
    viennacl::copy(vcl_C, ublas_C1);
    std::cout << " - Checking result... ";
    bool check_ok = true;
    for (std::size_t i = 0; i < ublas_A.size1(); ++i)
    {
      for (std::size_t j = 0; j < ublas_A.size2(); ++j)
      {
        if ( std::fabs(ublas_C1(i,j) - ublas_C(i,j)) / ublas_C(i,j) > 1e-4 )
        {
          check_ok = false;
          break;
        }
      }
      if (!check_ok)
        break;
    }
    if (check_ok)
      std::cout << "[OK]" << std::endl << std::endl;
    else
      std::cout << "[FAILED]" << std::endl << std::endl;
  }
  std::cout << "!!!! TUTORIAL COMPLETED SUCCESSFULLY !!!!" << std::endl;
  return EXIT_SUCCESS;
}
