#include <assimp/cimport.h>
#include <stdio.h>
#include <iostream>
#include <assimp/Importer.hpp>
#include <assimp/Exporter.hpp>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

int main(int argc, char** argv)
{
    const char* filenameout = NULL;
    const char* filename = NULL;
    filename = argv[1];
    filenameout = argv[2];

    auto stream = aiGetPredefinedLogStream(aiDefaultLogStream_STDOUT,NULL);
    aiAttachLogStream(&stream);
    Assimp::Importer Importer;
    Assimp::Exporter Exporter;
    const aiImporterDesc *iformat = nullptr;

    // Check and validate the specified model file extension.
    // https://github.com/assimp/assimp/issues/3827  binary ply is broken, use rply
    // has to be on both the import and export list.
    const char* extension = strrchr(filenameout, '.');
    if (!extension) {
        std::cout <<"Please provide a file with a valid extension.\n";
        return 1;
    }
    if (AI_FALSE == aiIsExtensionSupported(extension)) {
        std::cout <<"The specified model file extension is currently unsupported in Assimp\n ";
        return 1;
    }

    std::cout << "\tReading file using ASSIMP" << std::endl;
    const aiScene *aiscene = Importer.ReadFile(filename, aiProcess_ValidateDataStructure | aiProcessPreset_TargetRealtime_MaxQuality);
    if (!aiscene) {
        printf("Error parsing '%s': '%s'\n", filename, Importer.GetErrorString());
        return 1;}

    uint countin = Importer.GetImporterCount();
;
    uint ID = Importer.GetImporterIndex(extension);  //unfortunately these are not Exporter formatIDs
    int i = 0 ;
    do {
        iformat = Importer.GetImporterInfo(i);
        std::cout << "id: "<< i << " " << iformat->mFileExtensions << "\n";
        i++;
    } while (i < countin);


    std::cout << "ID: "<< ID << "\n";
    iformat = Importer.GetImporterInfo(ID);
    uint count = Exporter.GetExportFormatCount();
    std::cout << "count: "<< count << "\n";
    const aiExportFormatDesc *format;
    i = 0 ;
    do {
        format = Exporter.GetExportFormatDescription(i);
        std::cout << "id: "<< i << " " << format->id << "\n";
        if (iformat->mFileExtensions == format->id){
            std::cout << "ID is " << i << "\n";
            Exporter.Export(aiscene, format->id , filenameout, 0);
            std::cout << "Wrote " << filenameout << "\n";
        }
        i++;
    } while (i < count);

    if (!(aiReturn_SUCCESS == 0)) {
        std::cout << "Error exporting" << filenameout << Exporter.GetErrorString() << "\n" ;
    }

    aiDetachAllLogStreams();

    return 0;
}
