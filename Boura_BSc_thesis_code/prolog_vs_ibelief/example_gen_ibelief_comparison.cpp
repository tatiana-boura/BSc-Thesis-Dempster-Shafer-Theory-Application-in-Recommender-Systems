#include <iostream>
#include <fstream>
#include <random>
#include <vector>
#include <ctime>
#include <set>


bool exists(std::set<unsigned int> &A, std::vector<std::set<unsigned int> > &As){
    for(size_t i=0; i < As.size(); ++i)
        if( A == As[i] )
            return true;

    return false;
}


int main(int argc, char ** argv){
    unsigned int N = 10;    //size of Theta
    unsigned int M = 3; //number of mass functions
    unsigned int focal_points = 2; //number of focal points per mass funtion


    if( argc > 1)
        N = atoi(argv[1]);
    if( argc > 2)
        M = atoi(argv[2]);
    if( argc > 3)
        focal_points = atoi(argv[3]);

    std::string filename("../mass_func.pl");
    
    std::ofstream output;
    output.open(filename);

    std::random_device rd;
    std::mt19937 gen(time(NULL));
    std::uniform_int_distribution<> dis(1, N);
    std::uniform_real_distribution<double> real_dis(0.0, 1.0);

    output << ":-module(mass_func)." << std::endl << std::endl;
    output << ":-export(m/3)." << std::endl << std::endl;
    output << ":-export(theta/1)." << std::endl << std::endl;
    output << ":-export(num_of_m/1)." << std::endl << std::endl;


    output << "theta([1";
    for(unsigned int i=2; i <= N; ++i)
        output << "," << i;
    output << "])." << std::endl << std::endl;

    output << "num_of_m(" << M << ")." << std::endl << std::endl;

    for(unsigned int i=1; i <= M; ++i){  //for each mass function

        //std::cout << "here" << std::endl;

        double res = 1.0;
        std::vector<std::set<unsigned int>> As;

        for(unsigned j=1; j <= focal_points; ++j){    //for each focal point
            std::set<unsigned int> A;
            //pick size for A
            do{
                A.clear();
                unsigned int A_size = dis(gen);
                while( A.size() < A_size )
                    A.insert(dis(gen));
            }while( exists(A, As) );

            double A_m;
            if( j < focal_points ){
                A_m = real_dis( gen ) * res;
                if( A_m == 0.0 )
                    A_m = 0.1;
                res -= A_m;
            }
            else{
                A_m = res;
            }
            As.push_back(A);          
            
            output << "m(" << i << ", [";
            std::set<unsigned int>::iterator it=A.begin();
            output << *it;
            ++it;
            for(; it != A.end(); ++it)
                output << "," << *it;
            output << "], " << A_m << ")." << std::endl;
        }
        output << std::endl;
        As.clear();
    }
    output.close();


    return 0;
}