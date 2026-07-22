// c++ to be called by c

extern "C" int gnuplot_load(const char *iname);
#include "gnuplot-iostream/gnuplot-iostream.h"

int gnuplot_load(const char *iname) {

#ifdef _WIN32
    if (system(NULL)) puts (" gnuplot available");
    else exit (EXIT_FAILURE);
    if(system("cmd -v gnuplot > NUL 2>&1") ){
        std::cout << "'gnuplot' command is not available.\n";
        return 1;
    }
#else
    if (system(NULL)) puts (" gnuplot available");
    else exit (EXIT_FAILURE);
    if(system("command -v gnuplot > /dev/null 2>&1") ){
        std::cout << "'gnuplot' command is not available.\n";
        return 1;
    }
#endif

    Gnuplot gp;
    gp << "load \"" << iname << "\n";

#ifdef _WIN32
    // For Windows, prompt for a keystroke before the Gnuplot object goes out of scope so that
    // the gnuplot window doesn't get closed.
    std::cout << "Press enter to exit." << std::endl;
    std::cin.get();
#endif
    return(0);
}
