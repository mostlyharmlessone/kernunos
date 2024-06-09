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
    in vec3 vert;
    in vec3 vertNormal;
    uniform vec3 lightPos;
    void main()
    {
      vec3 L = normalize(lightPos - vert);
      float NL = max(dot(normalize(vertNormal), L), 0.0);
      vec3 col = clamp(outColor * 0.2 + outColor * 0.8 * NL, 0.0, 1.0);
      fragColor = vec4(outColor, 1.0);   //doesn't chnage the color based on normals
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
bool GLwidget::m_redraw = false;

bool GLwidget::m_centerNode = false;
bool GLwidget::m_adjustradii = false;
bool GLwidget::m_cubic = false;
bool GLwidget::m_LSQfillin = false;
bool GLwidget::m_Splinefillin = false;

bool GLwidget::m_Axial = true;
bool GLwidget::m_Tangential = false;
bool GLwidget::m_Instantaneous = false;
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

void GLwidget::cleanup()
{
  if (shaderProgram == nullptr)
            return;
  makeCurrent();
  glDeleteBuffers(1, &vertexbuffer);
  glDeleteBuffers(1,&elementbuffer);
  killTimer(timerID);
  delete shaderProgram;
  shaderProgram = nullptr;
  delete shaderGeoProgram;
  shaderGeoProgram = nullptr;
  delete shaderNormalProgram;
  shaderNormalProgram = nullptr;
  //deallocates Fortran arrays
  flag=flag-(flag%100)+99;  // last two digits of flag = 99;
  std::cout << "flag in cleanup: " << flag << "\n";
  QTemporaryFile file;
  QString fileName = file.fileName();
  DataPrint(fileName);
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
  m_parent->SetGLString(sglVer);

  glClearColor(0.2f, 0.3f, 0.3f, m_transparent ? 0 : 1);

  // Enable depth test
  glEnable(GL_DEPTH_TEST);
  // Accept fragment if it is closer to the camera than the former one
  glDepthFunc(GL_LESS);

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
  mViewMatrix.scale(QVector3D(0.005,0.005,0.005));
  mViewMatrix.translate(QVector3D(0,0,-1000));

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

  // Light position is fixed
  shaderProgram->setUniformValue(m_lightPosLoc, QVector3D(0, 0, 1000));
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
  shaderNormalProgram->setUniformValue(m_lightPosLoc, QVector3D(0, 0, 1000));
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
  shaderGeoProgram->setUniformValue(m_lightPosLoc, QVector3D(0, 0, 1000));
  shaderGeoProgram->release();

  // Create a Vertex Buffer Object
  glGenBuffers(1, &vertexbuffer);
  // Create an element array
  glGenBuffers(1, &elementbuffer);
}

bool GLwidget::DataPrint(QString fileName)
{
  QByteArray ba = fileName.toLocal8Bit();
  filename = ba.data();

  if (!((flag%100) == 0)){
    // reload values to avoid seg fault if previous nV and nE are too small.. and besides, they're not static!
    nV=51840;
    nE=26130;

    if (!((flag%100) == 1)){
      auto future1 = std::async([&]{return janus_(&flag, filename, elements, vertices, &nV, &nE);});  //everybody else gets blocking thread
      future1.get();}    
    else {
      std::thread([&]{return janus_(&flag, filename, elements, vertices, &nV, &nE);}).detach();}  //zern gets independent thread
  }
  return true;
}

bool GLwidget::DataLoad(QString fileName, bool first_time)  //first_time->cube
{
   //  the demo cube
    int nV_cube = 72;
    int nE_cube = 36;

    GLfloat cube_vertices[] = {
        -50.0f,  50.0f, -50.0f, -0.76f,  0.76f, -0.76f, 1.0f, 0.0f, 0.0f,  // Top-left & Red (x,y,z,nx,ny,nz,r,g,b)
         50.0f,  50.0f, -50.0f,  0.76f,  0.76f, -0.76f, 0.0f, 1.0f, 0.0f,    // Top-right & Green
         50.0f, -50.0f, -50.0f,  0.76f, -0.76f, -0.76f, 0.0f, 0.0f, 1.0f,    // Bottom-right & Blue
        -50.0f, -50.0f, -50.0f, -0.76f, -0.76f, -0.76f, 1.0f, 1.0f, 1.0f,   // Bottom-left & White
        -50.0f,  50.0f,  50.0f, -0.76f,  0.76f,  0.76f, 1.0f, 1.0f, 0.0f,   // Top-left & Orange?
         50.0f,  50.0f,  50.0f,  0.76f,  0.76f,  0.76f, 0.0f, 1.0f, 1.0f,   // Top-right & Yellow?
         50.0f, -50.0f,  50.0f,  0.76f, -0.76f,  0.76f, 1.0f, 0.0f, 1.0f,    // Bottom-right & Pink?
        -50.0f, -50.0f,  50.0f, -0.76f, -0.76f,  0.76f, 0.0f, 0.0f, 0.0f     // Bottom-left & Black
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


    QByteArray ba = fileName.toLocal8Bit();
    filename = ba.data();
//    std::cout << "filename in C++ in DataLoad: " << filename << std::endl;
//    std::cout << "first_time: " << first_time << std::endl;


    if (!first_time)
     {
      // reload values to avoid seg fault if previous nV and nE are too small.. and besides, they're not static, nor can they be!
      nV=51840;
      nE=26130;

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
      futureWatcher.setFuture(QtConcurrent::run([&]{return janus_(&flag, filename, elements, vertices, &nV, &nE);}));

      // Display the dialog and start the event loop.
      dialog.exec();

      futureWatcher.waitForFinished();

      // Query the future to check if was canceled.
      qDebug() << "Canceled?" << futureWatcher.future().isCanceled();

    //   blocks!
    //  std::future future1 = std::async([&]{return janus_(&flag, filename, elements, vertices, &nV, &nE);});
    //  future1.get();

    //  std::thread([&]{return janus_(&flag, filename, elements, vertices, &nV, &nE);}).detach();  //no blocking thread

    //      janus_(&flag, filename, elements, vertices, &nV, &nE);
     }
    else
     {
      nV=nV_cube;
      nE=nE_cube;
      //arrays have to be assigned this way vertices=cube_vertices only works in the same scope
      //fortran array handling seems a lot more consistently intuitive, to say nothing of the whole static idiocy in c++
      // however in non-DEBUG compilation the program fails to load data without error or crash and has the following warnings
      // warning: iteration 72 (36 in the elements loop) invokes undefined behavior [-Waggressive-loop-optimizations]
      // and void*_builtin_memcpy(void*,const void*,long unsigned int) reading 292 bytes froma region of size 288 (or 148 from 144 in the elements loop)
      for (int i=0; i<= nV; ++i){
      vertices[i]=cube_vertices[i];
      }
      for (int i=0; i<= nE; ++i){
      elements[i]=cube_elements[i];
      }
     }

    paintme=true;

  return true;
}


bool GLwidget::LoadSurfaceToBuffer(int nV, int nE, GLfloat* vertices, GLuint* elements)
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
    return true;
}

void GLwidget::paintGL(void)
{
    if ( !success ) return;  //not until shaders are built
    if ( !paintme ) return;  //not until nV, nE, vertices, elements are loaded
//     if ( !loaded ) return;  //not until nV, nE, vertices, elements are loaded
    if (!LoadSurfaceToBuffer(nV, nE, vertices, elements)) return;

    // Clear the screen
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

 // Bind buffers
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffer);

    m_world.setToIdentity();
    m_world.rotate(180.0f - (m_xRot / 16.0f), 1, 0, 0);
    m_world.rotate(m_yRot / 16.0f, 0, 1, 0);
    m_world.rotate(m_zRot / 16.0f, 0, 0, 1);
    QMatrix4x4 mMVP =  mViewMatrix  * m_world;

    // Use shader or shaderNormal
    if (m_lighting) {
    shaderNormalProgram->bind();
    // Send our transformation to the currently bound shader,
    // in the "mMVP" uniform
    shaderNormalProgram->setUniformValue(m_viewMatrixLoc, mMVP);
    shaderNormalProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
    glDrawElements(GL_TRIANGLES, nE, GL_UNSIGNED_INT, 0);
    // Unbind shader
    shaderNormalProgram->release();
    } else {
    shaderProgram->bind();
    // Send our transformation to the currently bound shader,
    // in the "mMVP" uniform
    shaderProgram->setUniformValue(m_viewMatrixLoc, mMVP);
    shaderProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
    glDrawElements(GL_TRIANGLES, nE, GL_UNSIGNED_INT, 0);
    // Unbind shader
    shaderProgram->release();
    };
    // draw normals here
    if (m_normal) {
    shaderGeoProgram->bind();
    // Send our transformation to the currently bound shader,
    // in the "mMVP" uniform
    shaderGeoProgram->setUniformValue(m_viewMatrixLoc, mMVP);
    shaderGeoProgram->setUniformValue(m_projMatrixLoc, projectionMatrix);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, nE);  //nV?
    // Unbind shader
    shaderGeoProgram->release();
    };

    // Unbind buffers
    glBindBuffer(vertexbuffer,0);
    glBindBuffer(elementbuffer,0);
}

// refreshes the window
void GLwidget::timerEvent(QTimerEvent*)
{
 update();
}

void GLwidget::resizeGL(int w, int h)
{
  projectionMatrix.setToIdentity();
  projectionMatrix.perspective(45.0f, GLfloat(w) / h, 0.01f, 100.0f);
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
     mViewMatrix.scale(QVector3D(0.01,0.01,0.01));
     update();
    break;
    case Qt::Key_N:  /*  N Key */
     mViewMatrix.scale(QVector3D(50,50,50));
     update();
    break;    
    case Qt::Key_Q:  /*  Q Key */
     mViewMatrix.translate(50*QVector3D(0,0,-0.1));
     update();
      break;
  case Qt::Key_S:  /*  S Key */
     mViewMatrix.translate(50*QVector3D(0,0,0.1));
     update();
      break;
  case Qt::Key_W:  /*  W Key */
     mViewMatrix.translate(50*QVector3D(0,0.1,0));
     update();
      break;
  case Qt::Key_X:  /*  X Key */
     mViewMatrix.translate(50*QVector3D(0,-0.1,0));
     update();
      break;
  case Qt::Key_A:  /*  A Key */
     mViewMatrix.translate(50*QVector3D(-0.1,0,0));
     update();
      break;
  case Qt::Key_D:  /*  D Key */
     mViewMatrix.translate(50*QVector3D(0.1,0,0));
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
         mViewMatrix.translate(500*QVector3D(0,0,-0.1));
         update();
         }
         else {
         mViewMatrix.translate(500*QVector3D(0,0,0.1));
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
   return QSize(50, 50);
}

QSize GLwidget::sizeHint() const
{
   return QSize(400, 400);
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


