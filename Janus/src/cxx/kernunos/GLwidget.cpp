#include "GLwidget.h"
#include "kernunos.h"
#include "qtconcurrentrun.h"


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


// defaults
bool GLwidget::m_transparent = false;
bool GLwidget::m_normal = false;
bool GLwidget::m_lighting = false;
bool GLwidget::m_pupilshow = false;
bool GLwidget::m_redraw = false;

bool GLwidget::m_centerNode = false;
bool GLwidget::m_adjustradii = false;
bool GLwidget::m_cubic = false;
bool GLwidget::m_LSQfillin = false;
bool GLwidget::m_Splinefillin = false;
bool GLwidget::m_pupilregister = false;
bool GLwidget::m_decenter = false;
bool GLwidget::m_consistency = false;
bool GLwidget::m_lsqvsspline = true;
bool GLwidget::m_2dspline = false;
bool GLwidget::m_axisymmetric = true;

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
bool GLwidget::m_perceptualuniformpalette = false;

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

void GLwidget::checkGLError()
{
    GLenum err;
    while(((err = glGetError()) != GL_NO_ERROR)){
            std::cout << err << std::endl;
        }
}

void GLwidget::cleanup()
{
  if (shaderProgram == nullptr)
            return;
  makeCurrent();
  glDeleteBuffers(4,vertexbuffers);
  glDeleteBuffers(4,elementbuffers);
  killTimer(timerID);
  delete shaderProgram;
  shaderProgram = nullptr;
  delete shaderGeoProgram;
  shaderGeoProgram = nullptr;
  delete shaderNormalProgram;
  shaderNormalProgram = nullptr;
  m_vao.destroy();
  //deallocates Fortran arrays
  flag=flag-(flag%100)+99;  // last two digits of flag = 99;
//  std::cout << "flag in cleanup: " << flag << "\n";
  #ifdef _WIN32
//  system("copy zernike.tmp zernike.bak");
  system("del zernike.tmp");   //probably broken if zernike not computed, as there is no zernike.tmp file
  #endif
  #ifndef _WIN32
  system("touch zernike.tmp");
//  system("cp zernike.tmp zernike.bak");
  system("rm zernike.tmp");
  #endif
  doneCurrent();
}

void GLwidget::initializeGL()
{
  // initialize OpenGL
  initializeOpenGLFunctions();

  // Get the GL version
  QString sglVer = "\nUsing OpenGL version: ";
  const GLubyte* GLversion = glGetString(GL_VERSION);
  const GLubyte* GLvendor =glGetString(GL_VENDOR);
  const GLubyte* GLrenderer =glGetString(GL_RENDERER);
  sglVer += reinterpret_cast<const char *>(GLversion);
  sglVer += "\nVendor: ";
  sglVer += reinterpret_cast<const char *>(GLvendor);
  sglVer += "\nRenderer: ";
  sglVer += reinterpret_cast<const char *>(GLrenderer);

  // all this below to track OpenGl errors becasuse Qt doesnt have glDebugMessageCallback
  QSurfaceFormat format;
  format.setMajorVersion(4);
  format.setMinorVersion(5);
  format.setProfile(QSurfaceFormat::CoreProfile);
  format.setOption(QSurfaceFormat::DebugContext);
  QOpenGLContext *context = new QOpenGLContext;
  context->setFormat(format);
  context->create();

  QOpenGLContext *ctx = QOpenGLContext::currentContext();
  QOpenGLDebugLogger *logger = new QOpenGLDebugLogger(this);
  logger->initialize();
  ctx->hasExtension(QByteArrayLiteral("GL_KHR_debug"));
  const QList<QOpenGLDebugMessage> messages = logger->loggedMessages();
  for (const QOpenGLDebugMessage &message : messages)
  qDebug() << message;
  qDebug() << "You started kernunos from a commandline";  //this only shows up if starting from a commandline

  glEnable              ( GL_DEBUG_OUTPUT );
//  glDebugMessageCallback( MessageCallback, 0 );   //weird that Qt can't seem to find glDebugMessageCallback and defining MessageCallback in header leads to linking error

  glClearColor(0.2f, 0.3f, 0.3f, m_transparent ? 0 : 1);
  // Enable depth test; Accept fragment if it is closer to the camera than the former one
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);

  m_vao.create();
  m_vao.bind();

  // Create buffers
  glGenBuffers(4, vertexbuffers);
  glGenBuffers(4, elementbuffers);

  m_parent->SetGLString(sglVer);
  shaderProgram = new QOpenGLShaderProgram;
  shaderGeoProgram = new QOpenGLShaderProgram;
  shaderNormalProgram = new QOpenGLShaderProgram;
  // load and compile vertex shaders
  success = shaderProgram->addShaderFromSourceCode(QOpenGLShader::Vertex,vertexSource);
  success = success && shaderNormalProgram->addShaderFromSourceCode(QOpenGLShader::Vertex,vertexSource);
  success = success && shaderGeoProgram->addShaderFromSourceCode(QOpenGLShader::Vertex,vertexGeoSource);
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

  //link the programs
  shaderProgram->link();
  shaderNormalProgram->link();
  shaderGeoProgram->link();

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

  m_vao.release();

}

bool GLwidget::Swap()
{
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

    //  the demo cube
    int nV_cube = 72;
    int nE_cube = 36;

    GLfloat cube_vertices[] = {
        -50.0f,  50.0f, -50.0f, -0.76f,  0.76f, -0.76f, 1.0f, 0.0f, 0.0f,   // Top-left & Red (x,y,z,nx,ny,nz,r,g,b)
        50.0f,  50.0f, -50.0f,  0.76f,  0.76f, -0.76f, 0.0f, 1.0f, 0.0f,   // Top-right & Green
        50.0f, -50.0f, -50.0f,  0.76f, -0.76f, -0.76f, 0.0f, 0.0f, 1.0f,   // Bottom-right & Blue
        -50.0f, -50.0f, -50.0f, -0.76f, -0.76f, -0.76f, 1.0f, 1.0f, 1.0f,   // Bottom-left & White
        -50.0f,  50.0f,  50.0f, -0.76f,  0.76f,  0.76f, 1.0f, 1.0f, 0.0f,   // Top-left & Orange?
        50.0f,  50.0f,  50.0f,  0.76f,  0.76f,  0.76f, 0.0f, 1.0f, 1.0f,   // Top-right & Yellow?
        50.0f, -50.0f,  50.0f,  0.76f, -0.76f,  0.76f, 1.0f, 0.0f, 1.0f,   // Bottom-right & Pink?
        -50.0f, -50.0f,  50.0f, -0.76f, -0.76f,  0.76f, 0.0f, 0.0f, 0.0f    // Bottom-left & Black
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

      // blocks!
      // Start the computation.
      paintme=false;
      if ((flag%100) == 10){
          auto future1 = std::async([&]{return janus_(&flag,filename,elements3,vertices3,legend2,zern,&nV[2],&nE[2],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);});
          future1.get();}
      else {

          nV[1]=nV[0]; nE[1]=nE[0];
          for (int i=0; i < nV[1]; ++i){
              vertices2[i]=vertices[i];
          }
          for (int i=0; i< nE[1]; ++i){
              elements2[i]=elements[i];
          }

          futureWatcher.setFuture(QtConcurrent::run([&]{return janus_(&flag,filename,elements,vertices,legend,zern,&nV[0],&nE[0],&nL,pupil_elements,pupil_vertices,&pupil_nV,&pupil_nE, &err_janus);}));
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
           -50.0f,  50.0f, -50.0f, -0.76f,  0.76f, -0.76f, 1.0f, 0.0f, 0.0f,   // Top-left & Red (x,y,z,nx,ny,nz,r,g,b)
            50.0f,  50.0f, -50.0f,  0.76f,  0.76f, -0.76f, 0.0f, 1.0f, 0.0f,   // Top-right & Green
            50.0f, -50.0f, -50.0f,  0.76f, -0.76f, -0.76f, 0.0f, 0.0f, 1.0f,   // Bottom-right & Blue
           -50.0f, -50.0f, -50.0f, -0.76f, -0.76f, -0.76f, 1.0f, 1.0f, 1.0f,   // Bottom-left & White
           -50.0f,  50.0f,  50.0f, -0.76f,  0.76f,  0.76f, 1.0f, 1.0f, 0.0f,   // Top-left & Orange?
            50.0f,  50.0f,  50.0f,  0.76f,  0.76f,  0.76f, 0.0f, 1.0f, 1.0f,   // Top-right & Yellow?
            50.0f, -50.0f,  50.0f,  0.76f, -0.76f,  0.76f, 1.0f, 0.0f, 1.0f,   // Bottom-right & Pink?
           -50.0f, -50.0f,  50.0f, -0.76f, -0.76f,  0.76f, 0.0f, 0.0f, 0.0f    // Bottom-left & Black
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
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(GLfloat), nullptr);
                                                           // offset 0, 9 floats = 3 positions + 3 normals+ 3 colors per vertex
    // vertex normals
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(GLfloat), reinterpret_cast<void *>(3 * sizeof(GLfloat)));
                                                           // offset 3 because normals start after 3 positions.
    // color attribute
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 9 * sizeof(GLfloat), reinterpret_cast<void *>(6 * sizeof(GLfloat)));
                                                          // offset 6 because colors start after 3 positions + 3 normals

//    checkGLError();

    // Unbind buffer
    glBindBuffer(vertexbuffer,0);
    glBindBuffer(elementbuffer,0);

    return true;
}

void GLwidget::paintGL(void)
{
    if ( !success ) return;  //not until shaders are built
    if ( !paintme ) return;  //not until nV, nE, vertices, elements are loaded
    if ( err_janus != 0 ) return;

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
    for (int i=4; i > 0 ; i--)
    {
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vertexbuffers[i]);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffers[i]);
    //kludgy
    QMatrix4x4 mMVP;
    QVector4D m_alpha;
    if (i == 1) {
        if (!LoadSurfaceToBuffer(nV[i-1], nE[i-1], vertexbuffers[i], elementbuffers[i], vertices, elements)) return;
        m_world.setToIdentity();
        m_world.rotate(180.0f - (m_xRot / 16.0f), 1, 0, 0);
        m_world.rotate(m_yRot / 16.0f, 0, 1, 0);
        m_world.rotate(m_zRot / 16.0f, 0, 0, 1);
        mMVP =  mViewMatrix  * m_world;
        m_alpha = QVector4D(0,0,0,m_alpha_value);
    }
    if (i == 2) {
        if (!LoadSurfaceToBuffer(nV[i-1], nE[i-1], vertexbuffers[i], elementbuffers[i], vertices2, elements2)) return;
        mMVP.setToIdentity();
        mMVP.scale(scale*QVector3D(1.0,1.0,1.0)/10000.0);
        mMVP.translate(QVector3D(-900,0,-position));
        m_alpha = QVector4D(0,0,0,1.0);
    }
    if (i == 3) {
        if (!LoadSurfaceToBuffer(nV[i-1], nE[i-1], vertexbuffers[i], elementbuffers[i], vertices3, elements3)) return;
        mMVP.setToIdentity();
        mMVP.scale(scale*QVector3D(1.0,1.0,1.0)/10000.0);
        mMVP.translate(QVector3D(900,0,-position));
        m_alpha = QVector4D(0,0,0,1.0);
    }
    if (i == 4 && m_pupilshow) {
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

    if (i > 0) {
        // Use shader or shaderNormal
        if (m_lighting) {
            shaderNormalProgram->bind();
            shaderNormalProgram->setUniformValue(m_viewMatrixLoc, mMVP);
            shaderNormalProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
            glDrawElements(GL_TRIANGLES, nE[i-1], GL_UNSIGNED_INT, 0);
            // Unbind shader
            shaderNormalProgram->release();
        } else {
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
    // Unbind buffers
    glBindBuffer(vertexbuffers[i],0);
    glBindBuffer(elementbuffers[i],0);
    }
    m_vao.release();
    }
}

// refreshes the window
void GLwidget::timerEvent(QTimerEvent*)
{
 update();
}

void GLwidget::resizeGL(int w, int h)
{
  projectionMatrix.setToIdentity();
  projectionMatrix.perspective(5.0f, GLfloat(w) / h, 0.01f, 20000.0f);  // I really don't want to have the side images appear too tilted away from the center
  update();
}

void GLwidget::keyPressEvent(QKeyEvent *e)
{
  switch (e->key())
  {
    case Qt::Key_Escape:  /*  Escape Key */
     exit(0);
    break;
    case Qt::Key_M:  /*  M Key */
     mViewMatrix.scale(0.7*QVector3D(1.0,1.0,1.0)/scale);
     update();
    break;
    case Qt::Key_N:  /*  N Key */
     mViewMatrix.scale(QVector3D(1.0,1.0,1.0)*scale);
     update();
    break;    
    case Qt::Key_Q:  /*  Q Key */
     mViewMatrix.translate(scale*QVector3D(0,0,-0.1));
     update();
      break;
  case Qt::Key_S:  /*  S Key */
     mViewMatrix.translate(scale*QVector3D(0,0,0.1));
     update();
      break;
  case Qt::Key_W:  /*  W Key */
     mViewMatrix.translate(scale*QVector3D(0,0.1,0));
     update();
      break;
  case Qt::Key_X:  /*  X Key */
     mViewMatrix.translate(scale*QVector3D(0,-0.1,0));
     update();
      break;
  case Qt::Key_A:  /*  A Key */
     mViewMatrix.translate(scale*QVector3D(-0.1,0,0));
     update();
      break;
  case Qt::Key_D:  /*  D Key */
     mViewMatrix.translate(scale*QVector3D(0.1,0,0));
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

         if (numPixels.y() > 0) {
         mViewMatrix.translate(scale*QVector3D(0,0,-1.0));
         update();
         }
         else {
         mViewMatrix.translate(scale*QVector3D(0,0,1.0));
         update();
         }
    e->accept();
}

void GLwidget::mousePressEvent(QMouseEvent *e)
{
    m_lastPos = e->position().toPoint();
}

void GLwidget::mouseMoveEvent(QMouseEvent *e)
{
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
   qNormalizeAngle(angle);
   if (angle != m_xRot) {
       m_xRot = angle;
       emit xRotationChanged(angle);
       update();
   }
}

void GLwidget::setYRotation(int angle)
{
   qNormalizeAngle(angle);
   if (angle != m_yRot) {
       m_yRot = angle;
       emit yRotationChanged(angle);
       update();
   }
}

void GLwidget::setZRotation(int angle)
{
   qNormalizeAngle(angle);
   if (angle != m_zRot) {
       m_zRot = angle;
       emit zRotationChanged(angle);
       update();
   }
}

void GLwidget::settransparency(int percent)
{
    float percent_fract=percent/100.0;
    if (percent_fract != m_alpha_value) {
        m_alpha_value = percent_fract;
        emit transparencyChanged(percent);
        update();
    }
}


