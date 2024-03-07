#include <assimp/cimport.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <assimp/Importer.hpp>
#include <assimp/Exporter.hpp>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

int main(int argc, char** argv)
{
    std::string filename = "float-color.ply";

 //   auto stream = aiGetPredefinedLogStream(aiDefaultLogStream_STDOUT,NULL);
 //   aiAttachLogStream(&stream);

    Assimp::Importer Importer;
    //Importer.
    std::cout << "\tReading file using ASSIMP" << std::endl;
    const aiScene *aiscene = Importer.ReadFile(filename.c_str(), aiProcess_ValidateDataStructure | aiProcessPreset_TargetRealtime_MaxQuality);

    std::string str = Importer.GetErrorString();


    if (!aiscene) {
        printf("Error parsing '%s': '%s'\n", filename.c_str(), Importer.GetErrorString());
        return 1;
    }


    Assimp::Exporter Exporter;
//    const aiExportFormatDesc* format = Exporter.GetExportFormatDescription(0);
    const aiExportFormatDesc* format = Exporter.GetExportFormatDescription(0);
    int lIndex = filename.find_last_of('/');

    //const string path = Filename.substr(0,lIndex+1);
    std::string path = "float-color.off";
    std::cout << "\tExport path: " << path << std::endl;

    Assimp::ExportProperties *properties = new Assimp::ExportProperties;
    properties->SetPropertyBool(AI_CONFIG_EXPORT_POINT_CLOUDS, true);
//    aiReturn ret = Exporter.Export(aiscene, "off", path, 0, properties );  //makes no error and no file
//    aiReturn ret = Exporter.Export(aiscene, "off", path);  //no error and no file
 //   aiReturn ret = Exporter.Export(aiscene, "off", "float-color.off");  //no error and no file
    aiReturn ret = Exporter.Export(aiscene, format->id, path, 0);  //id makes bad file with error, description,extension make no file, no error

    if (!(aiReturn_SUCCESS == 0)) {
        printf("Error exporting '%s': '%s'\n", filename.c_str(), Exporter.GetErrorString());
        return 1;
    }


    return 0;
}
