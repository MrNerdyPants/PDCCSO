/**
 *  Tests to validate the LSGO Scala implementation
 *  using the C++ implementation as reference
 */

#include "Header.h"
#include "Benchmarks.h"
#include <iostream>
#include <fstream>
#include <chrono>
#include <random>
#include <string>

unsigned dim = 1000;

/* benchmark functions */
unsigned funNum = 15;
unsigned funToRun[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
double lowLimit[] = {-100.0,-5.0,-32.0,-100.0,-5.0,-32.0,-100.0,-100.0,-5.0,-32.0,-100.0,-100.0,-100.0,-100.0,-100.0};
double upLimit[] = {100.0,5.0,32.0,100.0,5.0,32.0,100.0,100.0,5.0,32.0,100.0,100.0,100.0,100.0,100.0};

/* Benchmarks subclass used to change the visibility of basic functions */
class BasicFuns: public Benchmarks{
 public:
   using Benchmarks::sphere;
   using Benchmarks::elliptic;
   using Benchmarks::rastrigin;
   using Benchmarks::ackley;
   using Benchmarks::schwefel;
   using Benchmarks::rosenbrock;
};
unsigned basicFunNum = 6;
// store pointers to basic functions methods
double (BasicFuns::*basicFunToRun[])(double*, int) = {&BasicFuns::sphere, &BasicFuns::elliptic, &BasicFuns::rastrigin, &BasicFuns::ackley, &BasicFuns::schwefel, &BasicFuns::rosenbrock};
string basicFunName[] = {"sphere", "elliptic", "rastrigin", "ackley", "schwefel", "rosenbrock"};

unsigned samples = 1;
unsigned precision = 18;

double* X = new double[dim];
double* XX = new double[dim];
double f = 0.0;

Benchmarks* fp=NULL;

// base dir for cdatafiles and output files
string path ="/tmp";

/* Random values */
unsigned seed = std::chrono::system_clock::now().time_since_epoch().count(); // get a seed from the system clock
std::mt19937 generator(seed);
std::uniform_real_distribution<double> distribution(0.0,1.0);
auto randomValue = std::bind(distribution, generator);

/* Common functions  --------------------------------------------------- */

// calculate precision: smallest value e such that 1.0 + e > 1.0
void calculatePrecision(void){
    double e = 1.0;
    while (1.0 + e > 1.0)
        e *= 0.5;
    printf("Precision = %.36E\n", e*2.0);
}

// calculate f_id(X)
double calculateFitness(unsigned id, double* X){
  fp = generateFuncObj(funToRun[id]);
  return fp->compute(X);
}

// initialize the global X vector with random values
double* randomVector(void){
  for (unsigned pos=0; pos<dim; pos++)
    X[pos] = randomValue();
  return X;
}

// write vector X to the output stream data
void write(std::ofstream& data, double* X){
  for (unsigned pos=0; data.good() && pos<dim; pos++)
    data << X[pos] << std::endl;
}

// scale vector X to f_i limits. The result is stored in global XX vector
double* scale(unsigned i, double* X){
  for (unsigned k=0; k<dim; k++)
    XX[k] = lowLimit[i] + X[k] * (upLimit[i] - lowLimit[i]);
  return XX;
}

/* Tests -------------------------------------------------------------- */

// for every benchmark function calculate f(xopt)
void testOptimum(void){
  for (unsigned i=0; i<funNum; i++){
    // open the xopt file
    char file[30];
    sprintf(file, "/cdatafiles/F%d-xopt.txt", i+1);
    std::ifstream data(path + file);
    if (!data.is_open())
      break;

    // read the xopt vector
    for (unsigned pos=0; data.good() && pos<dim; pos++)
      if (data >> X[pos])
        XX[pos] = X[pos];

    // calculate fitness
    f = calculateFitness(i, XX);
    printf("F%d: %1.16g\n", i+1, f);

    data.close();
  }
}

// for every benchmark function calculate f(zero)
void testZero(void){
  // initialize the zero vector
  for (unsigned pos=0; pos<dim; pos++)
    X[pos] = 0.0;

  // calculate fitness
  for (unsigned i=0; i<funNum; i++){
    f = calculateFitness(i, X);

    printf("F%d: %1.16g\n", i+1, f);
  }
}

// for every basic function calculate f(X) for a number of random samples
// random samples and fitness values are written to a file
void testBasicFuns(void){

   BasicFuns bFuns; // needed to call method pointers stored in basicFunToRun

  // open the output file, write the number of samples and adjust precision
  std::ofstream data(path + "/lsgo-basicfuns.txt");
  if (!data.is_open()) return;
  data << samples << std::endl;
  data.precision(precision);

  for (unsigned i=0; data.good() && i<basicFunNum; i++) {

    std::cout << "[Function: " << basicFunName[i] << "]" << std::endl;

    for (unsigned sample=0; data.good() && sample<samples; sample++){
        // initialize and write the random vector
        X = randomVector();
        write(data, X);

        // calculate fitness and write it to output file and standard output
        f = (&bFuns->*basicFunToRun[i])(X, dim);
        data << f << std::endl;
        printf("%d: %1.16g\n", sample+1, f);
    }
  }
  data.close();
}

// for each in a number of random samples calculate f(X) for every benchmark function
// random samples and fitness values are written to a file
void testRandom(void){
  // open the output file, write the number of samples and adjust precision
  std::ofstream data(path + "/lsgo-random.txt");
  if (!data.is_open()) return;
  data << samples << std::endl;
  data.precision(precision);

  for (unsigned sample=0; data.good() && sample<samples; sample++){
    // initialize and write the random vector
    X = randomVector();
    write(data, X);

    printf("[Sample: %d]\n", sample+1);
    for (unsigned i=0; data.good() && i<funNum; i++) {
        // scale the input vector to function limits and calculate fitness
        XX = scale(i, X);
        f = calculateFitness(i, XX);

        // write fitness to output file and standard output
        data << f << std::endl;
        printf("F%d: %1.16g\n", i+1, f);
      }
  }
  data.close();
}

// for every benchmark function calculate f(X) for a number of random samples
// random samples and fitness values are written to a file
void testRandomByFun(void){
  // open the output file, write the number of samples and adjust precision
  std::ofstream data(path + "/lsgo-randombyfun.txt");
  if (!data.is_open()) return;
  data << samples << std::endl;
  data.precision(precision);

  for (unsigned i=0; data.good() && i<funNum; i++) {

    printf("[Function: %d]\n", i+1);

    for (unsigned sample=0; data.good() && sample<samples; sample++){
        // initialize and write the random vector
        X = randomVector();
        write(data, X);

        // scale the input vector to function limits and calculate fitness
        XX = scale(i, X);
        f = calculateFitness(i, XX);

        // write fitness to output file and standard output
        data << f << std::endl;
        printf("%d: %1.16g\n", sample+1, f);
    }
  }
  data.close();
}

int main(int argc, char** argv) {

  if(argc>=4 || argc<=1){
    printf("Usage: test id [samples]\n");
    return -1;
  }
  int test = stoi(argv[1]);
  if (argc==3)
    samples = stoi(argv[2]);

  switch (test){
    case 1:  printf("============= Optimum =============\n");
             testOptimum();
             break;
    case 2:  printf("============== Zero ===============\n");
             calculatePrecision();
             testZero();
             break;
    case 3:  printf("== Basic Functions (samples: %d) ==\n", samples);
             testBasicFuns();
             break;
    case 4:  printf("====== Random (samples: %d) =======\n", samples);
             testRandom();
             break;
    case 5:  printf("= Random by Function (samples: %d) =\n", samples);
             testRandomByFun();
             break;
    default: printf("Unknown test ID. Valid values are: 1:Optimum, 2:Zero, 3:BasicFuns, 4:Random, 5:RandomByFun.\n");
  }

  delete []X;
  delete []XX;

  return 0;
}

// create new object of class with default setting
Benchmarks* generateFuncObj(int funcID){
  Benchmarks *fp;
  // run each of specified function in "configure.ini"
  if (funcID==1){
    fp = new F1();
  }else if (funcID==2){
    fp = new F2();
  }else if (funcID==3){
    fp = new F3();
  }else if (funcID==4){
    fp = new F4();
  }else if (funcID==5){
    fp = new F5();
  }else if (funcID==6){
    fp = new F6();
  }else if (funcID==7){
    fp = new F7();
  }else if (funcID==8){
    fp = new F8();
  }else if (funcID==9){
    fp = new F9();
  }else if (funcID==10){
    fp = new F10();
  }else if (funcID==11){
    fp = new F11();
  }else if (funcID==12){
    fp = new F12();
  }else if (funcID==13){
    fp = new F13();
  }else if (funcID==14){
    fp = new F14();
  }else if (funcID==15){
    fp = new F15();
  }else{
    cerr<<"Fail to locate Specified Function Index"<<endl;
    exit(-1);
  }
  return fp;
}