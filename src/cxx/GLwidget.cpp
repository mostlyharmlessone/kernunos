#include "GLwidget.h"
#include "kernunos.h"
//#include "qtconcurrentrun.h"

#include <iostream>
#include <string>

#include <ft2build.h>
#include FT_FREETYPE_H

using std::vector;

static const GLchar* vertexSource = R"glsl(
 #version 330 core
    in vec3 position;   // the position variable has attribute position 0
    in vec3 normal; // the normal variable has attribute position 1
    in vec3 incolor; // the color variable has attribute position 2
    out vec3 outColor;  // output a color to the fragment shader
    out vec3 vert;
    out vec3 vertNormal;
    uniform mat4 mMVP;
    uniform mat4 projectionMatrix;

    void main()
    {
       vert=position;
       mat3 normalMatrix = mat3(transpose(inverse(mMVP)));
       vertNormal = normalize(vec3(vec4(normalMatrix * normal, 0.0)));
       gl_Position = projectionMatrix * mMVP * vec4(position, 1.0);
       outColor = incolor; // set outColor to the input color we got from the vertex data
    }
)glsl";

static const GLchar* vertexGeoSource = R"glsl(
 #version 330 core
    in vec3 position;   // the position variable has attribute position 0
    in vec3 normal; // the normal variable has attribute position 1
    in vec3 incolor; // the color variable has attribute position 2

    out VS_OUT {
      vec3 normal;
      vec3 color;
    } vs_out;

    uniform mat4 mMVP;

    void main()
    {
       mat3 normalMatrix = mat3(transpose(inverse(mMVP)));
       vs_out.normal = normalize(vec3(vec4(normalMatrix * normal, 0.0)));
       vs_out.color = incolor;
       gl_Position = mMVP * vec4(position, 1.0);
    }
)glsl";

static const GLchar* geometrySource = R"glsl(
#version 330 core
layout (triangles) in;
layout (line_strip, max_vertices = 6) out;

in VS_OUT {
    vec3 normal;
    vec3 color;
} gs_in[];

const float MAGNITUDE = 0.1;
uniform mat4 projectionMatrix;

void GenerateLine(int index)
{
    gl_Position =  projectionMatrix * gl_in[index].gl_Position;
    EmitVertex();
    gl_Position = projectionMatrix * (gl_in[index].gl_Position +
                               vec4(gs_in[index].normal, 0.0) * MAGNITUDE);
    EmitVertex();
    EndPrimitive();
}

void main()
{
   GenerateLine(0); // first vertex normal
   GenerateLine(1); // second vertex normal
   GenerateLine(2); // third vertex normal
}
)glsl";

static const GLchar* fragmentGeoSource = R"glsl(
#version 330 core
in vec3 outColor;
out vec4 fragColor;

void main()
{
    fragColor = vec4(0.0, 0.0, 0.0, 0.0);  //black normals
}
)glsl";

static const GLchar* fragmentColor = R"glsl(
 #version 330 core
    out vec4 fragColor;
    in vec3 outColor;
    uniform vec4 alpha;
    void main()
    {
      fragColor = vec4(outColor, 0.0) + alpha;
    }
)glsl";

static const GLchar* fragmentColorNormal = R"glsl(
 #version 330 core
    out vec4 fragColor;
    in vec3 outColor;
    in vec3 vert;
    in vec3 vertNormal;
    uniform vec3 lightPos;
    void main()
    {
      vec3 L = normalize(lightPos - vert);
      float NL = max(dot(normalize(vertNormal), L), 0.0);
      vec3 col = clamp(outColor * 0.2 + outColor * 0.8 * NL, 0.0, 1.0);
      fragColor = vec4(col, 1.0);
    }
)glsl";

static const GLchar* textfsrc = R"glsl(
#version 330 core
    in vec2 texpos;
    uniform sampler2D tex;
    uniform vec4 color;
    out vec4 FragColor;

    void main(void) {
    FragColor = vec4(1, 1, 1, texture(tex, texpos).a) * color;
    }
)glsl";

static const GLchar* textvsrc = R"glsl(
#version 330 core
    in vec4 coord;
    out vec2 texpos;
    uniform mat4 mMVP;
    uniform mat4 projection;

    void main(void) {
     gl_Position = projection * mMVP * vec4(coord.xy, 0, 1.0);
     texpos = coord.zw;
    }
)glsl";


// defaults
bool GLwidget::m_transparent = false;
bool GLwidget::m_normal = false;
bool GLwidget::m_lighting = false;
bool GLwidget::m_pupilshow = false;
bool GLwidget::m_axesshow = false;
bool GLwidget::m_anglesshow = false;
bool GLwidget::m_powershow = false;
bool GLwidget::m_redraw = false;

bool GLwidget::m_centerNode = false;
bool GLwidget::m_adjustradii = false;
bool GLwidget::m_cubic = false;
bool GLwidget::m_LSQfillin = false;
bool GLwidget::m_Splinefillin = false;
bool GLwidget::m_pupilregister = false;
bool GLwidget::m_decenter = false;
bool GLwidget::m_cropping = false;
bool GLwidget::m_consistency = false;
bool GLwidget::m_lsqvsspline = true;
bool GLwidget::m_LSQspline = true;
bool GLwidget::m_axisymmetric = true;
bool GLwidget::m_notelevation = false;

bool GLwidget::m_Axial = true;
bool GLwidget::m_Oblique = false;
bool GLwidget::m_Tangential = false;
bool GLwidget::m_Gaussian = false;
bool GLwidget::m_Mean = false;
bool GLwidget::m_MongeAstig = false;
bool GLwidget::m_Elevation = false;
bool GLwidget::Z44VerticalQuatrafoil = false;
bool GLwidget::Z42Vertical2ndAstig = false;
bool GLwidget::Z40SphericalAberration = false;
bool GLwidget::Z4neg2Oblique2ndAstig = false;
bool GLwidget::Z4neg4ObliqueQuatrafoil = false;
bool GLwidget::Z33ObliqueTrefoil = false;
bool GLwidget::Z3neg3VerticalTrefoil = false;
bool GLwidget::Z31HorizontalComa = false;
bool GLwidget::Z3neg1VerticalComa = false;
bool GLwidget::Z22VerticalAstig = false;
bool GLwidget::Z2neg2ObliqueAstig = false;
bool GLwidget::Z20Defocus = false;
bool GLwidget::Z11Xtilt = false;
bool GLwidget::Z1neg1Ytilt = false;
bool GLwidget::Z00Piston = false;

bool GLwidget::m_rgb2 = false;
bool GLwidget::m_rgb5 = false;
bool GLwidget::m_hsbrgb = false;
bool GLwidget::m_gplotpalette = false;
bool GLwidget::m_USSfixed = true;
bool GLwidget::m_perceptualuniformfixed = false;
bool GLwidget::m_USSpalette = false;
bool GLwidget::m_USSNIDEK = false;
bool GLwidget::m_perceptualuniformpalette = false;


GLint attribute_coord;
GLint uniform_tex;
GLint uniform_color;

struct point {
    GLfloat x;
    GLfloat y;
    GLfloat s;
    GLfloat t;
};

FT_Library ft;
FT_Face face;

float sx = 2.0 / SCR_WIDTH ;
float sy = 2.0 / SCR_HEIGHT;

//https://stackoverflow.com/questions/2125880/convert-float-to-stdstring-in-c
template <typename T> std::string to_str(const T& t) {
    std::ostringstream os;
    os<<t;
    return os.str();
}

GLwidget::GLwidget ( QWidget *parent ) : QOpenGLWidget(parent)
{
        // --transparent causes the clear color to be transparent. Therefore, on systems that
        // support it, the widget will become transparent apart from the display window.
        if (m_transparent) {
            QSurfaceFormat fmt = format();
            fmt.setAlphaBufferSize(8);
            setFormat(fmt);
        }
  setFocusPolicy(Qt::StrongFocus);
  timerID = startTimer(100);
}

GLwidget::~GLwidget()
{
  cleanup();
}

//Use by inserting:  checkGLError(__FILE__, __LINE__);
void GLwidget::checkGLError(const char* file, int line) {
    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR) {
        std::cout << "OpenGL Error " << err << "  at " << file << ":" << line << std::endl;
    }
}

void GLwidget::cleanup()
{
  if (shaderProgram == nullptr)
            return;
  makeCurrent();
  glDeleteBuffers(8,vertexbuffers);
  glDeleteBuffers(8,elementbuffers);
  killTimer(timerID);
  delete shaderProgram;
  shaderProgram = nullptr;
  delete shaderGeoProgram;
  shaderGeoProgram = nullptr;
  delete shaderNormalProgram;
  shaderNormalProgram = nullptr;
  m_vao.release();
  m_vao.destroy();
  //deallocates Fortran arrays
  flag=flag-(flag%100)+99;  // last two digits of flag = 99;
//  std::cout << "flag in cleanup: " << flag << "\n";
  #ifdef _WIN32  //use %temp% here?
//  system("copy zernike.tmp zernike.bak");
  system("type nul >> zernike.tmp");
  system("del zernike.tmp");
  system("del /tmp/kernunos_temp.XXXXX.*");
  #else
  system("touch /tmp/zernike.tmp");
//  system("cp /tmp/zernike.tmp /tmp/zernike.bak");
  system("rm /tmp/zernike.tmp");
  system("rm /tmp/kernunos_temp.XXXXX.*");
  #endif
  doneCurrent();
}

void GLwidget::initializeGL()
{
  // initialize OpenGL
  initializeOpenGLFunctions();
  // Get the GL version
  GLint major = 0, minor = 0;
  glGetIntegerv(GL_MAJOR_VERSION, &major);
  glGetIntegerv(GL_MINOR_VERSION, &minor);
  if (major*100 + minor*10 < 330) {
      std::cout << "Kernunos needs GLSL 3.30 or greater " << std::endl;
      cleanup();
      QWidget::close();
      std::exit(0);  // not the cleanest exit
  }
  QString sglVer = "\nUsing OpenGL version: ";
  const GLubyte* GLversion = glGetString(GL_VERSION);
  const GLubyte* GLvendor =glGetString(GL_VENDOR);
  const GLubyte* GLrenderer =glGetString(GL_RENDERER);
  const GLubyte* GLSLversion =glGetString(GL_SHADING_LANGUAGE_VERSION);
  sglVer += reinterpret_cast<const char *>(GLversion);
  sglVer += "\nVendor: ";
  sglVer += reinterpret_cast<const char *>(GLvendor);
  sglVer += "\nRenderer: ";
  sglVer += reinterpret_cast<const char *>(GLrenderer);
  sglVer += "\nGLSL version: ";
  sglVer += reinterpret_cast<const char *>(GLSLversion);
//throws an error under Apple/Darwin, does nothing under others
  QOpenGLContext *ctx = QOpenGLContext::currentContext();
  QOpenGLDebugLogger *logger = new QOpenGLDebugLogger(this);
  logger->initialize();
  logger->startLogging();
  ctx->hasExtension(QByteArrayLiteral("GL_KHR_debug"));
  const QList<QOpenGLDebugMessage> messages = logger->loggedMessages();
  for (const QOpenGLDebugMessage &message : messages)
  qDebug() << message;

  LogC(("Widget OpenGl: " + std::to_string(format().majorVersion()) + "." + std::to_string(format().minorVersion())).c_str());
  LogC(("Context valid: " + std::to_string(context()->isValid())).c_str());
  LogC(("Really used OpenGl: " + std::to_string(context()->format().majorVersion()) + "." + std::to_string(context()->format().minorVersion())).c_str());
#if defined(__APPLE__)
  std::string verstring = sglVer.toUtf8().constData();
#else
  std::string verstring = sglVer.toStdString();
#endif
//  LogC(verstring.c_str());

  // During init, enable debug output
#if defined(__APPLE__)
  //not for Darwin
#else
  glEnable(GL_DEBUG_OUTPUT );
#endif
//  QOpenGLExtraFunctions::glDebugMessageCallback(MessageCallback, 0 ); //cant get this to work

  glClearColor(0.2f, 0.3f, 0.3f, m_transparent ? 0 : 1);
  // Enable depth test; Accept fragment if it is closer to the camera than the former one
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);

  m_vao.create();
  m_vao.bind();

  // Create buffers
  glGenBuffers(8, vertexbuffers);
  glGenBuffers(8, elementbuffers);

  m_parent->SetGLString(sglVer);
  shaderProgram = new QOpenGLShaderProgram;
  shaderGeoProgram = new QOpenGLShaderProgram;
  shaderNormalProgram = new QOpenGLShaderProgram;
  shaderText2Program = new QOpenGLShaderProgram;
  // load and compile vertex shaders
  success = shaderProgram->addShaderFromSourceCode(QOpenGLShader::Vertex,vertexSource);
  success = success && shaderNormalProgram->addShaderFromSourceCode(QOpenGLShader::Vertex,vertexSource);
  success = success && shaderGeoProgram->addShaderFromSourceCode(QOpenGLShader::Vertex,vertexGeoSource);
  success = success && shaderText2Program->addShaderFromSourceCode(QOpenGLShader::Vertex,textvsrc);
  //if (success) std::cout << "Compiled vertex shader" << std::endl;
  if (!success)
  {
    std::cout << "DID NOT compile vertex shader" << std::endl;
    QWidget::close();  //just closes the OpenGL widget
   }

  // load and compile geometry shader
  success = shaderGeoProgram->addShaderFromSourceCode(QOpenGLShader::Geometry,geometrySource);
  //if (success) std::cout << "Compiled geometry shader" << std::endl;
  if (!success)
  {
    std::cout << "DID NOT compile geometry shader" << std::endl;
    QWidget::close();  //just closes the OpenGL widget
  }

  // load and compile fragment shader; optional normals used for lighting
  // success = shaderProgram->addShaderFromSourceCode(QOpenGLShader::Fragment, m_normal ? fragmentColorNormal : fragmentColor);
  success = shaderProgram->addShaderFromSourceCode(QOpenGLShader::Fragment, fragmentColor);
  success = success && shaderNormalProgram->addShaderFromSourceCode(QOpenGLShader::Fragment, fragmentColorNormal);
  success = success && shaderGeoProgram->addShaderFromSourceCode(QOpenGLShader::Fragment, fragmentGeoSource);
  success = success && shaderText2Program->addShaderFromSourceCode(QOpenGLShader::Fragment,textfsrc);
  //if (success) std::cout << "Compiled fragment shader" << std::endl;
  if (!success)
  {
    std::cout << "DID NOT compile fragment shader" << std::endl;
    QWidget::close();
   }
  // proper distance & scale for cube
  mViewMatrix.setToIdentity();
  mViewMatrix.scale(scale*QVector3D(1.0,1.0,1.0)/10000.0);
  mViewMatrix.translate(QVector3D(0,0,-position));
  //unscaled version for text
   mUnscaledViewMatrix.setToIdentity();

  //link the programs
  shaderProgram->link();
  shaderNormalProgram->link();
  shaderGeoProgram->link();
  shaderText2Program->link();

  //set regular shader program up
  shaderProgram->bindAttributeLocation("position", 0);
  shaderProgram->bindAttributeLocation("normal", 1);
  shaderProgram->bindAttributeLocation("incolor", 2);

  shaderProgram->bind();
  m_viewMatrixLoc = shaderProgram->uniformLocation("mMVP");
  m_projMatrixLoc = shaderProgram->uniformLocation("projectionMatrix");
  m_lightPosLoc = shaderProgram->uniformLocation("lightPos");
  m_alphaLoc = shaderProgram->uniformLocation("alpha");

  // Light position is fixed
  shaderProgram->setUniformValue(m_lightPosLoc, QVector3D(0, 0, lightdist));
  shaderProgram->setUniformValue(m_alphaLoc, QVector4D(0,0,0,1.0));
  shaderProgram->release();


  // set up Text shader program
  shaderText2Program->bind();
  attribute_coord = shaderText2Program->attributeLocation("coord");
  uniform_tex = shaderText2Program->uniformLocation("tex");
  uniform_color = shaderText2Program->uniformLocation("color");
  m_viewMatrix2Loc = shaderText2Program->uniformLocation("mMVP");
  m_projectionLoc = shaderText2Program->uniformLocation("projection");
  shaderText2Program->release();

  //set light/normal shader program up
  shaderNormalProgram->bindAttributeLocation("position", 0);
  shaderNormalProgram->bindAttributeLocation("normal", 1);
  shaderNormalProgram->bindAttributeLocation("incolor", 2);

  shaderNormalProgram->bind();
  m_viewMatrixLoc = shaderNormalProgram->uniformLocation("mMVP");
  m_projMatrixLoc = shaderNormalProgram->uniformLocation("projectionMatrix");
  m_lightPosLoc = shaderNormalProgram->uniformLocation("lightPos");

  // Light position is fixed
  shaderNormalProgram->setUniformValue(m_lightPosLoc, QVector3D(0, 0, lightdist));
  shaderNormalProgram->release();

  // Now for the normals
  shaderGeoProgram->bindAttributeLocation("position", 0);
  shaderGeoProgram->bindAttributeLocation("normal", 1);
  shaderGeoProgram->bindAttributeLocation("incolor", 2);

  shaderGeoProgram->bind();
  m_viewMatrixLoc = shaderGeoProgram->uniformLocation("mMVP");
  m_projMatrixLoc = shaderGeoProgram->uniformLocation("projectionMatrix");
  m_lightPosLoc = shaderGeoProgram->uniformLocation("lightPos");

  // Light position is fixed
  shaderGeoProgram->setUniformValue(m_lightPosLoc, QVector3D(0, 0, lightdist));
  shaderGeoProgram->release();
}


bool GLwidget::Swap()
{
// Avoid swapping with the cube
   if (nV[1] > 100) {
    nV[2]=nV[0]; nE[2]=nE[0];
    for (int i=0; i < 51840; ++i){
        vertices3[i]=vertices[i];
    }
    for (int i=0; i< 26130; ++i){
        elements3[i]=elements[i];
    }
    nV[0]=nV[1]; nE[0]=nE[1];
    for (int i=0; i < 51840; ++i){
        vertices[i]=vertices2[i];
    }
    for (int i=0; i< 26130; ++i){
        elements[i]=elements2[i];
    }
    nV[1]=nV[2]; nE[1]=nE[2];
    for (int i=0; i < 51840; ++i){
        vertices2[i]=vertices3[i];
    }
    for (int i=0; i< 26130; ++i){
        elements2[i]=elements3[i];
    }

    //pupil data wiped out

    pupil_nV2=pupil_nV; pupil_nE2=pupil_nE;
    for (int i=0; i < pupil_nV; ++i){
        pupil_vertices2[i]=pupil_vertices[i];
        pupil_vertices[i]=0;
    }
    for (int i=0; i< pupil_nE; ++i){
        pupil_elements2[i]=pupil_elements[i];
        pupil_elements[i]=0;
    }


    //  the demo cube
    int nV_cube = 72;
    int nE_cube = 36;

    GLfloat cube_vertices[] = {
        -150.0f,  150.0f, -150.0f, -0.76f,  0.76f, -0.76f, 1.0f, 0.0f, 0.0f,   // Top-left & Red (x,y,z,nx,ny,nz,r,g,b)
        150.0f,  150.0f, -150.0f,  0.76f,  0.76f, -0.76f, 0.0f, 1.0f, 0.0f,   // Top-right & Green
        150.0f, -150.0f, -150.0f,  0.76f, -0.76f, -0.76f, 0.0f, 0.0f, 1.0f,   // Bottom-right & Blue
        -150.0f, -150.0f, -150.0f, -0.76f, -0.76f, -0.76f, 1.0f, 1.0f, 1.0f,   // Bottom-left & White
        -150.0f,  150.0f,  150.0f, -0.76f,  0.76f,  0.76f, 1.0f, 1.0f, 0.0f,   // Top-left & Orange?
        150.0f,  150.0f,  150.0f,  0.76f,  0.76f,  0.76f, 0.0f, 1.0f, 1.0f,   // Top-right & Yellow?
        150.0f, -150.0f,  150.0f,  0.76f, -0.76f,  0.76f, 1.0f, 0.0f, 1.0f,   // Bottom-right & Pink?
        -150.0f, -150.0f,  150.0f, -0.76f, -0.76f,  0.76f, 0.0f, 0.0f, 0.0f    // Bottom-left & Black
    };
    // 12 triangles = 6 faces with 2 triangles per face
    GLuint cube_elements[] = {
        0, 1, 2,
        2, 3, 0,
        4, 5, 6,
        6, 7, 4,
        0, 4, 5,
        5, 1, 0,
        3, 7, 6,
        6, 2, 3,
        0, 4, 7,
        7, 3, 0,
        1, 5, 6,
        6, 2, 1
    };

    nV[2]=nV_cube;
    nE[2]=nE_cube;

    for (int i=0; i< nV[2]; ++i){
        vertices3[i]=cube_vertices[i];
    }
    for (int i=0; i< nE[2]; ++i){
        elements3[i]=cube_elements[i];
    }

    paintme=true;
    return true;
    }
    else {
     std::cout << "No swapping with the cube" << std::endl;
     return false;}
}

bool GLwidget::DataLoad(QString fileName, bool filepresent)  //! filepresent->cube
{
    QByteArray ba = fileName.toLocal8Bit();
    filename = ba.data();

    if (filepresent)
     {

      // Create a progress dialog.
      QProgressDialog dialog;
      dialog.setLabelText(QString("Loading the data..."));
      // Create a QFutureWatcher and connect signals and slots.
      QFutureWatcher<void> futureWatcher;
      QObject::connect(&futureWatcher, &QFutureWatcher<void>::finished, &dialog, &QProgressDialog::reset);
      QObject::connect(&dialog, &QProgressDialog::canceled, &futureWatcher, &QFutureWatcher<void>::cancel);
      QObject::connect(&futureWatcher,  &QFutureWatcher<void>::progressRangeChanged, &dialog, &QProgressDialog::setRange);
      QObject::connect(&futureWatcher, &QFutureWatcher<void>::progressValueChanged,  &dialog, &QProgressDialog::setValue);

      // blocks but allows the dialog box!
      // Start the computation.
      paintme=false;
      if ((flag%100) == 10 || (flag%100) == 12){
//                                          janus_(&flag,filename,elements3,vertices3,legend2,cardinal2,zern,&nV[2],&nE[2],&nL,&nC,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);}
       auto future1 = std::async([&]{return janus_(&flag,filename,elements3,vertices3,legend2,cardinal2,zern,&nV[2],&nE[2],&nL,&nC,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);});
       future1.get();
      }
      else {

          nV[1]=nV[0]; nE[1]=nE[0];
          for (int i=0; i < nV[1]; ++i){
              vertices2[i]=vertices[i];
          }
          for (int i=0; i< nE[1]; ++i){
              elements2[i]=elements[i];
          }

//                                           janus_(&flag,filename,elements,vertices,legend,cardinal,zern,&nV[0],&nE[0],&nL,&nC,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);
//         auto future1 = std::async(std::launch::async,[&]{return janus_(&flag,filename,elements,vertices,legend,cardinal,zern,&nV[0],&nE[0],&nL,&nC,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);});
//       future1.get();

//         std::future<int> future1 = std::async(std::launch::async, [](){
//             janus_(&flag,filename,elements,vertices,legend,cardinal,zern,&nV[0],&nE[0],&nL,&nC,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);
//             return 1;
//         });
          futureWatcher.setFuture(QtConcurrent::run([&]{return janus_(&flag,filename,elements,vertices,legend,cardinal,zern,&nV[0],&nE[0],&nL,&nC,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);}));

//         auto future1 = std::async(std::launch::async,[&]{return janus_(&flag,filename,elements,vertices,legend,cardinal,zern,&nV[0],&nE[0],&nL,&nC,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);});

        // Display the dialog and start the event loop.
         dialog.exec();
        futureWatcher.waitForFinished();
          // Query the future to check if was canceled.
        qDebug() << "Canceled?" << futureWatcher.future().isCanceled();
        paintme=true;
      }
    }
    else
    {
        //  the demo cube
        int nV_cube = 72;
        int nE_cube = 36;

        GLfloat cube_vertices[] = {
            -150.0f,  150.0f, -150.0f, -0.76f,  0.76f, -0.76f, 1.0f, 0.0f, 0.0f,   // Top-left & Red (x,y,z,nx,ny,nz,r,g,b)
            150.0f,  150.0f, -150.0f,  0.76f,  0.76f, -0.76f, 0.0f, 1.0f, 0.0f,   // Top-right & Green
            150.0f, -150.0f, -150.0f,  0.76f, -0.76f, -0.76f, 0.0f, 0.0f, 1.0f,   // Bottom-right & Blue
            -150.0f, -150.0f, -150.0f, -0.76f, -0.76f, -0.76f, 1.0f, 1.0f, 1.0f,   // Bottom-left & White
            -150.0f,  150.0f,  150.0f, -0.76f,  0.76f,  0.76f, 1.0f, 1.0f, 0.0f,   // Top-left & Orange?
            150.0f,  150.0f,  150.0f,  0.76f,  0.76f,  0.76f, 0.0f, 1.0f, 1.0f,   // Top-right & Yellow?
            150.0f, -150.0f,  150.0f,  0.76f, -0.76f,  0.76f, 1.0f, 0.0f, 1.0f,   // Bottom-right & Pink?
            -150.0f, -150.0f,  150.0f, -0.76f, -0.76f,  0.76f, 0.0f, 0.0f, 0.0f    // Bottom-left & Black
        };
        // 12 triangles = 6 faces with 2 triangles per face
        GLuint cube_elements[] = {
            0, 1, 2,
            2, 3, 0,
            4, 5, 6,
            6, 7, 4,
            0, 4, 5,
            5, 1, 0,
            3, 7, 6,
            6, 2, 3,
            0, 4, 7,
            7, 3, 0,
            1, 5, 6,
            6, 2, 1
        };
      nV[0]=nV_cube;
      nE[0]=nE_cube;
      //arrays have to be assigned this way vertices=cube_vertices only works in the same scope
      //fortran array handling seems a lot more consistently intuitive, to say nothing of the whole static idiocy in c++
      // however in non-DEBUG compilation the program fails to load data without error or crash and has the following warnings
      // warning: iteration 72 (36 in the elements loop) invokes undefined behavior [-Waggressive-loop-optimizations]
      // and void*_builtin_memcpy(void*,const void*,long unsigned int) reading 292 bytes froma region of size 288 (or 148 from 144 in the elements loop)
      for (int i=0; i< nV[0]; ++i){
      vertices[i]=cube_vertices[i];
     // vertices2[i]=cube_vertices[i];
      }
      for (int i=0; i< nE[0]; ++i){
      elements[i]=cube_elements[i];
     // elements2[i]=cube_elements[i];
      }
      // USS starting values
      //
      legend[1]=255;
      legend[2]=238;
      legend[3]=248;
      legend[0]=68;

      legend[5]=255;
      legend[6]=216;
      legend[7]=226;
      legend[4]=66;

      legend[9]=255;
      legend[10]=196;
      legend[11]=206;
      legend[8]=65;

      legend[13]=255;
      legend[14]=175;
      legend[15]=186;
      legend[12]=63;

      legend[17]=255;
      legend[18]=157;
      legend[19]=167;
      legend[16]=62;

      legend[21]=255;
      legend[22]=138;
      legend[23]=148;
      legend[20]=60;

      legend[25]=255;
      legend[26]=114;
      legend[27]=124;
      legend[24]=59;

      legend[29]=255;
      legend[30]=95;
      legend[31]=105;
      legend[28]=57;

      legend[33]=255;
      legend[34]=70;
      legend[35]=79;
      legend[32]=56;

      legend[37]=255;
      legend[38]=40;
      legend[39]=50;
      legend[36]=54;

      legend[41]=255;
      legend[42]=0;
      legend[43]=0;
      legend[40]=53;

      legend[45]=254;
      legend[46]=102;
      legend[47]=0;
      legend[44]=51;

      legend[49]=252;
      legend[50]=153;
      legend[51]=0;
      legend[48]=50;

      legend[53]=252;
      legend[54]=188;
      legend[55]=0;
      legend[52]=48;

      legend[57]=254;
      legend[58]=254;
      legend[59]=0;
      legend[56]=47;

      legend[61]=162;
      legend[62]=250;
      legend[63]=58;
      legend[60]=45;

      legend[65]=79;
      legend[66]=229;
      legend[67]=51;
      legend[64]=44;

      legend[69]=51;
      legend[70]=204;
      legend[71]=51;
      legend[68]=42;

      legend[73]=32;
      legend[74]=176;
      legend[75]=71;
      legend[72]=41;

      legend[77]=0;
      legend[78]=152;
      legend[79]=102;
      legend[76]=39;

      legend[81]=0;
      legend[82]=106;
      legend[83]=156;
      legend[80]=38;

      legend[85]=0;
      legend[86]=50;
      legend[87]=204;
      legend[84]=36;

      legend[89]=0;
      legend[90]=0;
      legend[91]=203;
      legend[88]=35;

      legend[93]=0;
      legend[94]=0;
      legend[95]=153;
      legend[92]=33;

      legend[97]=0;
      legend[98]=0;
      legend[99]=111;
      legend[96]=32;

      legend[101]=0;
      legend[102]=0;
      legend[103]=80;
      legend[100]=30;

      //this is very much not the same as legend2=legend as it would be in fortran
      for (int i=0; i< nL; ++i){
          legend2[i]=legend[i];       //
      }
     }    
    paintme=true;
  return true;
}


bool GLwidget::LoadSurfaceToBuffer(int nV, int nE, GLuint vertexbuffer, GLuint elementbuffer, GLfloat* vertices, GLuint* elements)
{
    makeCurrent();
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    GLint data_size_in_bytes = sizeof(GLfloat)*nV;                              // 4-bytes per float x number of vertices
    glBufferData(GL_ARRAY_BUFFER, data_size_in_bytes, vertices, GL_STATIC_DRAW);
    GLint size = 0;
    glGetBufferParameteriv(GL_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);
    checkGLError(__FILE__, __LINE__);
     if(data_size_in_bytes != size)
      {
       glDeleteBuffers(1, &vertexbuffer);
       std::cout << "Error in vertices buffer: " << data_size_in_bytes << ", " << size << std::endl;
       return false;
      }
     if (!glIsBuffer(vertexbuffer)) return false;

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffer);
    data_size_in_bytes = sizeof(GLuint)*nE;                // 4-bytes per int x number of elements
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, data_size_in_bytes, elements, GL_STATIC_DRAW);
    size = 0;
    glGetBufferParameteriv(GL_ELEMENT_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);
     if(data_size_in_bytes != size)
       {
        glDeleteBuffers(1, &elementbuffer);
        std::cout << "Error in element buffer: " << data_size_in_bytes << ", " << size << std::endl;
        return false;
       }
     if (!glIsBuffer(elementbuffer)) return false;
    checkGLError(__FILE__, __LINE__);
    // positions, colors and normals all stored as floats: 9 * sizeof(GLfloat) = 3 x 3 floats
    // vertex position
    glEnableVertexAttribArray(0);
    checkGLError(__FILE__, __LINE__);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(GLfloat), nullptr);
                                                           // offset 0, 9 floats = 3 positions + 3 normals+ 3 colors per vertex
    checkGLError(__FILE__, __LINE__);
    // vertex normals
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(GLfloat), reinterpret_cast<void *>(3 * sizeof(GLfloat)));
                                                           // offset 3 because normals start after 3 positions.
        checkGLError(__FILE__, __LINE__);
    // color attribute
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(GLfloat), reinterpret_cast<void *>(6 * sizeof(GLfloat)));
                                                          // offset 6 because colors start after 3 positions + 3 normals
    checkGLError(__FILE__, __LINE__);
    // Unbind buffer; do not do this per Qt https://doc.qt.io/qt-6/qopenglwidget.html
//    glBindBuffer(vertexbuffer,0);
//    glBindBuffer(elementbuffer,0);
    return true;
}

// simplified version that just does lines with vertices and elements without normals or colors
bool GLwidget::LoadLinesToBuffer(int nV, int nE, GLuint vertexbuffer, GLuint elementbuffer, GLfloat* vertices, GLuint* elements)
{
    makeCurrent();
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    GLint data_size_in_bytes = sizeof(GLfloat)*nV;                              // 4-bytes per float x number of vertices
    glBufferData(GL_ARRAY_BUFFER, data_size_in_bytes, vertices, GL_STATIC_DRAW);
    GLint size = 0;
    glGetBufferParameteriv(GL_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);
    if(data_size_in_bytes != size)
    {
        glDeleteBuffers(1, &vertexbuffer);
        std::cout << "Error in vertices buffer: " << data_size_in_bytes << ", " << size << std::endl;
        return false;
    }
    if (!glIsBuffer(vertexbuffer)) return false;

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffer);
    data_size_in_bytes = sizeof(GLuint)*nE;                // 4-bytes per int x number of elements
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, data_size_in_bytes, elements, GL_STATIC_DRAW);
    size = 0;
    glGetBufferParameteriv(GL_ELEMENT_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);
    if(data_size_in_bytes != size)
    {
        glDeleteBuffers(1, &elementbuffer);
        std::cout << "Error in element buffer: " << data_size_in_bytes << ", " << size << std::endl;
        return false;
    }
    if (!glIsBuffer(elementbuffer)) return false;

    // positions, colors and normals all stored as floats: 9 * sizeof(GLfloat) = 3 x 3 floats
    // vertex position
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(GLfloat), nullptr);
    // offset 0, 3 floats = 3 positions

    return true;
}

// https://gitlab.com/wikibooks-opengl/modern-tutorials/-/blob/master/text01_intro/text.cpp?ref_type=heads
/*
 * Render text using the currently loaded font and currently set font size.
 * Rendering starts at coordinates (x, y), z is always 0.
 * The pixel coordinates that the FreeType2 library uses are scaled by (sx, sy).
 */
 // doen't handle rotation around z very well.
void GLwidget::render_text(GLuint vertexbuffer, const char *text, float x, float y, float sx, float sy) {
    makeCurrent();
    glUseProgram(shaderText2Program->programId());
    shaderText2Program->bind();
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
     /* Create a texture that will be used to hold one "glyph" */
    GLuint tex;
//__APPLE__
//    glActiveTexture(GL_TEXTURE0);
    glActiveTexture(GL_TEXTURE1);
    glGenTextures(1, &tex);
    glBindTexture(GL_TEXTURE_2D, tex);
//    glUniform1i(uniform_tex, 0);
    glUniform1i(uniform_tex, 1);
    /* We require 1 byte alignment when uploading texture data */
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    /* Clamping to edges is important to prevent artifacts when scaling */
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    /* Linear filtering usually looks best for text */
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    /* Set up the VBO for our vertex data */
    glEnableVertexAttribArray(attribute_coord);
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    glVertexAttribPointer(attribute_coord, 4, GL_FLOAT, GL_FALSE, 0, 0);
    // FreeType
    // --------
    FT_Library ft;
    // All functions return a value different than 0 whenever an error occurred
    if (FT_Init_FreeType(&ft))
    {
        std::cout << "ERROR::FREETYPE: Could not init FreeType Library" << std::endl;
        return;
    }
    // find path to font
#ifdef __APPLE__
    std::string font_name = "/System/Library/fonts/Geneva.ttf";
#else
#ifdef __WIN32
    std::string font_name = "\\Windows\\Fonts\\LiberationMono-Regular.ttf";
#else
    std::string font_name = "/usr/share/fonts/liberation/LiberationMono-Regular.ttf";
#endif
#endif
    if (font_name.empty())
    {
        std::cout << "ERROR::FREETYPE: Failed to load font_name" << std::endl;
        return;
    }
    // load font as face
    FT_Face face;
    if (FT_New_Face(ft, font_name.c_str(), 0, &face)) {
        std::cout << "ERROR::FREETYPE: Failed to load font" << std::endl;
        return;
    }
    else {
        // set size to load glyphs as
        FT_Set_Pixel_Sizes(face, 0, 48);
        // disable byte-alignment restriction
        glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
        // Load character glyph
        const char *p;
        FT_GlyphSlot g = face->glyph;
        /* Loop through all characters */
        for (p = text; *p; p++) {
          /* Try to load and render the character */
            if (FT_Load_Char(face, *p, FT_LOAD_RENDER))
             {
               std::cout << "ERROR::FREETYTPE: Failed to load Glyph" << std::endl;
               continue;
             }
            /* Upload the "bitmap", which contains an 8-bit grayscale image, as an alpha texture */
             glTexImage2D(GL_TEXTURE_2D, 0, GL_ALPHA, g->bitmap.width, g->bitmap.rows, 0, GL_ALPHA, GL_UNSIGNED_BYTE, g->bitmap.buffer);
             /* Calculate the vertex and texture coordinates */
             float x2 = x + g->bitmap_left * sx;
             float y2 = -y - g->bitmap_top * sy;
             float w = g->bitmap.width * sx;
             float h = g->bitmap.rows * sy;

             point box[4] = {
                             {x2, -y2, 0, 0},
                             {x2 + w, -y2, 1, 0},
                             {x2, -y2 - h, 0, 1},
                             {x2 + w, -y2 - h, 1, 1},
                             };

             /* Draw the character on the screen */
              glBufferData(GL_ARRAY_BUFFER, sizeof box, box, GL_DYNAMIC_DRAW);
              glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
             /* Advance the cursor to the start of the next character */
              x += (g->advance.x/64) * sx;
              y += (g->advance.y/64) * sy;
            }
            // set texture options
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    }
    glDeleteTextures(1, &tex);
}


void GLwidget::paintGL(void)
{
    if ( !success ) return;  //not until shaders are built
    if ( !paintme ) return;  //not until nV, nE, vertices, elements are loaded
    if ( err_janus != 0 ) return;

    makeCurrent();
    // Clear the screen    
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    if(!m_normal) {
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);   //incompatible with showing normals
        glBlendEquation(GL_FUNC_ADD);
        //glBlendEquation(GL_FUNC_SUBTRACT);
    } else{ glDisable(GL_BLEND);}

    //wireframe
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    //regular
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    m_vao.bind();
    // do them in this order for transparency overlay
    for (int i=7; i > 0 ; i--)
    {
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vertexbuffers[i]);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffers[i]);
    //kludgy
    QMatrix4x4 mMVP;
    QVector4D m_alpha;
    if (i == 1) {
        // draws central image, rotates. translates, scales
        if (!LoadSurfaceToBuffer(nV[i-1], nE[i-1], vertexbuffers[i], elementbuffers[i], vertices, elements)) return;
        m_world.setToIdentity();
        m_world.rotate(180.0f - (m_xRot / 16.0f), 1, 0, 0);
        m_world.rotate(m_yRot / 16.0f, 0, 1, 0);
        m_world.rotate(m_zRot / 16.0f, 0, 0, 1);
        mMVP =  mViewMatrix  * m_world;
        m_alpha = QVector4D(0,0,0,m_alpha_value);
    }

    if (i == 2) {
        //draws last image, does not rotate. translate, scale
        if (!LoadSurfaceToBuffer(nV[i-1], nE[i-1], vertexbuffers[i], elementbuffers[i], vertices2, elements2)) return;
        mMVP.setToIdentity();
        mMVP.scale(scale*QVector3D(1.0,1.0,1.0)/10000.0);
        mMVP.translate(QVector3D(-1200,0,-position));
        m_alpha = QVector4D(0,0,0,1.0);
    }

    if (i == 3) {
        //draws comparison, does not rotate. translate, scale
        if (!LoadSurfaceToBuffer(nV[i-1], nE[i-1], vertexbuffers[i], elementbuffers[i], vertices3, elements3)) return;
        mMVP.setToIdentity();
        mMVP.scale(scale*QVector3D(1.0,1.0,1.0)/10000.0);
        mMVP.translate(QVector3D(1200,0,-position));
        m_alpha = QVector4D(0,0,0,1.0);
    }

    if (i == 4 && m_pupilshow) {
        // displays pupil data as black 2D image arbitrarily behind corneal data, rotates. translates, scales
        if(!LoadSurfaceToBuffer(pupil_nV, pupil_nE, vertexbuffers[i-4], elementbuffers[i-4], pupil_vertices, pupil_elements)) return;
        m_world.setToIdentity();
        m_world.rotate(180.0f - (m_xRot / 16.0f), 1, 0, 0);
        m_world.rotate(m_yRot / 16.0f, 0, 1, 0);
        m_world.rotate(m_zRot / 16.0f, 0, 0, 1);
        mMVP =  mViewMatrix  * m_world;
        m_alpha = QVector4D(0,0,0,1.0);
        shaderProgram->bind();
        shaderProgram->setUniformValue(m_viewMatrixLoc, mMVP);
        //only the regular shader has the adjustable transparency for one buffer
        shaderProgram->setUniformValue(m_alphaLoc, m_alpha);
        shaderProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
        glDrawElements(GL_TRIANGLES, pupil_nE, GL_UNSIGNED_INT, 0);
        // Unbind shader
        shaderProgram->release();
    }
     if (i == 5 && m_axesshow) {
        // draws a set of three cartesian axes x=red, y= green, z = blue, rotates. translates, scales
        // what a f* of a lot of trouble to assign values to a vector for passing..
         /* Printing std vectors of integers (int) to console, isn't cpp straightforward? */
         //       https://stackoverflow.com/questions/10750057/how-do-i-print-out-the-contents-of-a-vector
         //        std::copy(axis_Vertices.begin(), axis_Vertices.end(), std::ostream_iterator<int>(std::cout, " "));
         //        std::copy(pupil_Vertices.begin(), pupil_Vertices.end(), std::ostream_iterator<int>(std::cout, " "));
         //        coming to C++ 23, not yet apparently supported, (Fortran has had vector printing for 50 years)
         //        std::print("{}", axis_Vertices);
        std::vector<GLfloat> axis_Vertices;
        std::vector<GLuint> axis_Elements;
        int axis_NE = 6;
        int axis_NV = 54;
        GLfloat array_axis_vertices[] = {0.0f, 0.0f, 0.0f,  0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f,  // Red/G/B (x,y,z,nx,ny,nz,r,g,b)
                                         0.0f, 0.0f, 0.0f,  0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f,
                                         0.0f, 0.0f, 0.0f,  0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f,
                                        700.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f,
                                        0.0f, 700.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f,
                                        0.0f, 0.0f, 700.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f};

        axis_Vertices.assign (array_axis_vertices,array_axis_vertices+axis_NV);   // assigning from array
        GLuint array_axis_elements[] = {0,3,  1,4,  2,5};
        axis_Elements.assign (array_axis_elements,array_axis_elements+axis_NE);
        GLfloat* axis_vertices = axis_Vertices.data();
        GLuint* axis_elements = axis_Elements.data();
        m_world.setToIdentity();
        m_world.rotate(180.0f - (m_xRot / 16.0f), 1, 0, 0);
        m_world.rotate(m_yRot / 16.0f, 0, 1, 0);
        m_world.rotate(m_zRot / 16.0f, 0, 0, 1);
        mMVP =  mViewMatrix  * m_world;
        m_alpha = QVector4D(0,0,0,1.0);
        shaderProgram->bind();
        shaderProgram->setUniformValue(m_viewMatrixLoc, mMVP);
        //only the regular shader has the adjustable transparency for one buffer
        shaderProgram->setUniformValue(m_alphaLoc, m_alpha);
        shaderProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
//      axes
        if(!LoadSurfaceToBuffer(axis_NV, axis_NE, vertexbuffers[i], elementbuffers[i], axis_vertices, axis_elements)) return;
         glDrawElements(GL_LINES, axis_NE, GL_UNSIGNED_INT, 0);
        shaderProgram->release();
     }

     if (i == 6 && m_anglesshow) {
//       draws a circle with degrees at each clock hour, not compatible with Normals, rotates. translates, scales
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         GLfloat white[4] = { 1, 1, 1, 1 };
//         GLfloat black[4] = { 0, 0, 0, 1 };
//         GLfloat red[4] = { 1, 0, 0, 1 };
         m_world.setToIdentity();
         m_world.rotate(180.0f - (m_xRot / 16.0f), 1, 0, 0);
         m_world.rotate(m_yRot / 16.0f, 0, 1, 0);
         m_world.rotate(m_zRot / 16.0f, 0, 0, 1);
         glm::mat4 projection = glm::ortho(-static_cast<float>(SCR_WIDTH)/SCR_HEIGHT, static_cast<float>(SCR_WIDTH)/SCR_HEIGHT, -1.0f, 1.0f);
         mMVP = mUnscaledViewMatrix * m_world;
         shaderText2Program->bind();
         shaderText2Program->setUniformValue(m_viewMatrix2Loc, mMVP);
         glUniformMatrix4fv(glGetUniformLocation(shaderText2Program->programId(), "projection"), 1, GL_FALSE, glm::value_ptr(projection));
         glUniform4fv(uniform_color, 1, white);
         for (int j=0; j < 12; ++j){
             std::string degrees  = to_str(180.0*j/6.0);
             render_text(vertexbuffers[i],degrees.c_str(), 0.8*cos(3.1415*j/6.0), 0.8*sin(3.1415*j/6.0), sx, sy);
         }
         shaderText2Program->release();
     }

     if (i == 7 && m_powershow) {
         //superpose values in diopters in 3 x 3 circular grid over image
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         GLfloat white[4] = { 1, 1, 1, 1 };
         // GLfloat black[4] = { 0, 0, 0, 1 };
         // GLfloat red[4] = { 1, 0, 0, 1 };
         // GLfloat transparent_green[4] = { 0, 1, 0, 0.5 };
         m_world.setToIdentity();
         m_world.rotate(180.0f - (m_xRot / 16.0f), 1, 0, 0);
         m_world.rotate(m_yRot / 16.0f, 0, 1, 0);
         m_world.rotate(m_zRot / 16.0f, 0, 0, 1);
         glm::mat4 projection = glm::ortho(-static_cast<float>(SCR_WIDTH)/SCR_HEIGHT, static_cast<float>(SCR_WIDTH)/SCR_HEIGHT, -1.0f, 1.0f);
         mMVP = mUnscaledViewMatrix * m_world;
         shaderText2Program->bind();
         shaderText2Program->setUniformValue(m_viewMatrix2Loc, mMVP);
         glUniformMatrix4fv(glGetUniformLocation(shaderText2Program->programId(), "projection"), 1, GL_FALSE, glm::value_ptr(projection));
         glUniform4fv(uniform_color, 1, white);

         // central value
         std::string degrees  = std::to_string(int(cardinal[0]));
         render_text(vertexbuffers[i],degrees.c_str(), 0.0, 0.0, sx, sy);
         // cardinal values
         for (int j=1; j < nC; ++j){
             degrees  = std::to_string(int(cardinal[j]));
             render_text(vertexbuffers[i],degrees.c_str(), 0.2*cos(3.14159*(j-1)/4.0), 0.2*sin(3.14159*(j-1)/4.0), sx, sy);
         }

         shaderText2Program->release();
     }

//  not pupil
    if (i > 0 && i < 4) {
        // Use shader or shaderNormal
        if (m_lighting) {
            shaderNormalProgram->bind();
            shaderNormalProgram->setUniformValue(m_viewMatrixLoc, mMVP);
            shaderNormalProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
            glDrawElements(GL_TRIANGLES, nE[i-1], GL_UNSIGNED_INT, 0);
            // Unbind shader
            shaderNormalProgram->release();}
            else {
            shaderProgram->bind();
            shaderProgram->setUniformValue(m_viewMatrixLoc, mMVP);
            //only the regular shader has the adjustable transparency for one buffer
            shaderProgram->setUniformValue(m_alphaLoc, m_alpha);
            shaderProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
            glDrawElements(GL_TRIANGLES, nE[i-1], GL_UNSIGNED_INT, 0);
            // Unbind shader
            shaderProgram->release();
        };
        // draw normals here
        if (m_normal) {
            shaderGeoProgram->bind();
            shaderGeoProgram->setUniformValue(m_viewMatrixLoc, mMVP);
            shaderGeoProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
            glDrawArrays(GL_TRIANGLE_STRIP, 0, nE[i-1]);
            // Unbind shader
            shaderGeoProgram->release();
        };     
        checkGLError(__FILE__, __LINE__);
    // Unbind buffer; do not do this per Qt https://doc.qt.io/qt-6/qopenglwidget.html
 //   glBindBuffer(vertexbuffers[i],0);
 //   glBindBuffer(elementbuffers[i],0);       
    }
  }
}

// refreshes the window
void GLwidget::timerEvent(QTimerEvent*)
{
 update();
}

void GLwidget::resizeGL(int w, int h)
{
  makeCurrent();
  projectionMatrix.setToIdentity();
  projectionMatrix.perspective(5.0f, GLfloat(w) / h, 0.01f, 20000.0f);  // I really don't want to have the side images appear too tilted away from the center
  update();
}

void GLwidget::keyPressEvent(QKeyEvent *e)
{
    makeCurrent();
    switch (e->key())
  {
    case Qt::Key_Escape:  /*  Escape Key */
     exit(0);
    break;
    case Qt::Key_M:  /*  M Key */
     mViewMatrix.scale(0.7*QVector3D(1.0,1.0,1.0)/scale);
     mUnscaledViewMatrix.scale(0.7*QVector3D(1.0,1.0,1.0)/scale);
     update();
    break;
    case Qt::Key_N:  /*  N Key */
     mViewMatrix.scale(QVector3D(1.0,1.0,1.0)*scale);
     mUnscaledViewMatrix.scale(QVector3D(1.0,1.0,1.0)*scale);
     update();
    break;    
    case Qt::Key_Q:  /*  Q Key */
     mViewMatrix.scale(QVector3D(1.0,1.0,1.0)/1.1);
     mUnscaledViewMatrix.scale(QVector3D(1.0,1.0,1.0)/1.1);
     update();
      break;
  case Qt::Key_S:  /*  S Key */
     mViewMatrix.scale(1.1*QVector3D(1.0,1.0,1.0));
     mUnscaledViewMatrix.scale(1.1*QVector3D(1.0,1.0,1.0));
     update();
      break;
  //try to match speed of degree circle with images
  case Qt::Key_W:  /*  W Key */
     mViewMatrix.translate(scale*QVector3D(0,0.1,0));
     mUnscaledViewMatrix.translate(0.063*QVector3D(0,0.1,0));
     update();
      break;
  case Qt::Key_X:  /*  X Key */
     mViewMatrix.translate(scale*QVector3D(0,-0.1,0));
     mUnscaledViewMatrix.translate(0.063*QVector3D(0,-0.1,0));
     update();
      break;
  case Qt::Key_A:  /*  A Key */
     mViewMatrix.translate(scale*QVector3D(-0.1,0,0));
     mUnscaledViewMatrix.translate(0.063*QVector3D(-0.1,0,0));
     update();
      break;
  case Qt::Key_D:  /*  D Key */
     mViewMatrix.translate(scale*QVector3D(0.1,0,0));
     mUnscaledViewMatrix.translate(0.063*QVector3D(0.1,0,0));
     update();
      break;
    default:
      break;
  }
  e->accept();  // Don't pass any key events to parent
}

void GLwidget::wheelEvent(QWheelEvent *e)
{
    QPoint numPixels = e->pixelDelta();
    QPoint numDegrees = e->angleDelta();
    if (!numPixels.isNull()) {
        makeCurrent();
        if (numPixels.y() > 0) {
            mViewMatrix.scale(QVector3D(1.0,1.0,1.0)/1.1);
            mUnscaledViewMatrix.scale(QVector3D(1.0,1.0,1.0)/1.1);
            update();
        }
        else {
            mViewMatrix.scale(1.1*QVector3D(1.0,1.0,1.0));
            mUnscaledViewMatrix.scale(1.1*QVector3D(1.0,1.0,1.0));
            update();
        }
    }
    else if (!numDegrees.isNull()) {
    makeCurrent();
        if (numDegrees.y() > 0) {
         mViewMatrix.scale(QVector3D(1.0,1.0,1.0)/1.1);
         mUnscaledViewMatrix.scale(QVector3D(1.0,1.0,1.0)/1.1);
         update();
         }
         else {
         mViewMatrix.scale(1.1*QVector3D(1.0,1.0,1.0));
         mUnscaledViewMatrix.scale(1.1*QVector3D(1.0,1.0,1.0));
         update();
         }
    }
    e->accept();
}


void GLwidget::mousePressEvent(QMouseEvent *e)
{
    makeCurrent();
    m_lastPos = e->position().toPoint();
}

void GLwidget::mouseMoveEvent(QMouseEvent *e)
{
    makeCurrent();
    int dx = e->position().toPoint().x() - m_lastPos.x();
    int dy = e->position().toPoint().y() - m_lastPos.y();

    if (e->buttons() & Qt::LeftButton) {
        setXRotation(m_xRot + 8 * dy);
        setYRotation(m_yRot + 8 * dx);
    } else if (e->buttons() & Qt::RightButton) {
        setXRotation(m_xRot + 8 * dy);
        setZRotation(m_zRot + 8 * dx);
    }
    m_lastPos = e->position().toPoint();
}


QSize GLwidget::minimumSizeHint() const
{
    return QSize(500, 250);
}

QSize GLwidget::sizeHint() const
{

    return QSize(SCR_WIDTH, SCR_HEIGHT);
}

static void qNormalizeAngle(int &angle)
{
    while (angle < 0)
       angle += 360 * 16;
   while (angle > 360 * 16)
       angle -= 360 * 16;
}

void GLwidget::setXRotation(int angle)
{
    makeCurrent();
    qNormalizeAngle(angle);
   if (angle != m_xRot) {
       m_xRot = angle;
       emit xRotationChanged(angle);
       update();
   }
}

void GLwidget::setYRotation(int angle)
{
    makeCurrent();
    qNormalizeAngle(angle);
   if (angle != m_yRot) {
       m_yRot = angle;
       emit yRotationChanged(angle);
       update();
   }
}

void GLwidget::setZRotation(int angle)
{
    makeCurrent();
    qNormalizeAngle(angle);
   if (angle != m_zRot) {
       m_zRot = angle;
       emit zRotationChanged(angle);
       update();
   }
}

void GLwidget::settransparency(int percent)
{
    makeCurrent();
    float percent_fract=percent/100.0;
    if (percent_fract != m_alpha_value) {
        m_alpha_value = percent_fract;
        emit transparencyChanged(percent);
        update();
    }
}


