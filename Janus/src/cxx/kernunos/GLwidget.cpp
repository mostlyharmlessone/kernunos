#include "GLwidget.h"
#include "kernunos.h"

static const GLchar* vertexSource = R"glsl(
#version 400 core
layout (location = 0) in vec3 position;   // the position variable has attribute position 0
layout (location = 1) in vec3 incolor; // the color variable has attribute position 1

out vec3 outColor; // output a color to the fragment shader

uniform mat4 mMVP;

void main()
{
    gl_Position = mMVP * vec4(position, 1.0);
    outColor = incolor; // set outColor to the input color we got from the vertex data
}
)glsl";

static const GLchar* fragmentSource = R"glsl(
#version 400 core
out vec4 fragColor;
in vec3 outColor;

void main()
{
    fragColor = vec4(outColor, 1.0);
}
)glsl";


bool GLwidget::m_transparent = false;

GLwidget::GLwidget ( QWidget *parent ) : QOpenGLWidget(parent)
{
        m_core = QSurfaceFormat::defaultFormat().profile() == QSurfaceFormat::CoreProfile;
        // --transparent causes the clear color to be transparent. Therefore, on systems that
        // support it, the widget will become transparent apart from the logo.
        if (m_transparent) {
            QSurfaceFormat fmt = format();
            fmt.setAlphaBufferSize(8);
            setFormat(fmt);
        }
  setFocusPolicy(Qt::StrongFocus);
  cameraPos = QVector3D(0, 0, 6);
  timerID = startTimer(1000);
}

GLwidget::~GLwidget()
{
  cleanup();
  glDeleteBuffers(1, &vertexbuffer);
  glDeleteBuffers(1,&elementbuffer);
  glDeleteProgram(programID);
  killTimer(timerID);
}

void GLwidget::cleanup()
{
  if (m_program == nullptr)
            return;
  makeCurrent();
  m_logoVbo.destroy();
  delete m_program;
  m_program = nullptr;
  doneCurrent();
  QObject::disconnect(context(), &QOpenGLContext::aboutToBeDestroyed, this, &GLwidget::cleanup);
}


void GLwidget::initializeGL()
{
  connect(context(), &QOpenGLContext::aboutToBeDestroyed, this, &GLwidget::cleanup);
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

  glClearColor(0.2f, 0.3f, 0.3f, 0.0f);

  // Enable depth test
  glEnable(GL_DEPTH_TEST);
  // Accept fragment if it is closer to the camera than the former one
  glDepthFunc(GL_LESS);

  // load and compile vertex shader
  success = shaderProgram.addShaderFromSourceCode(QOpenGLShader::Vertex,vertexSource);
  // if (success) std::cout << "Compiled vertex shader" << std::endl;
  if (!success)
  {
    std::cout << "DID NOT compile vertex shader" << std::endl;
    QWidget::close();  //just closes the OpenGL widget
   }

  // load and compile fragment shader
  success = shaderProgram.addShaderFromSourceCode(QOpenGLShader::Fragment,fragmentSource);
  //  if (success) std::cout << "Compiled fragment shader" << std::endl;
  if (!success)
  {
    std::cout << "DID NOT compile fragment shader" << std::endl;
    QWidget::close();
   }

  programID = shaderProgram.programId();

  // Projection matrix : 45° Field of View, 4:3 ratio, display range : 0.1 unit <-> 100 units
  mProjectionMatrix.perspective(45.0, 4.0/3.0, 0.1, 100.0);

  // Camera matrix
  mViewMatrix.lookAt( cameraPos, // Camera is at (0,0,6), in World Space
               QVector3D(0,0,0), // and looks at the origin
               QVector3D(0,1,0)  // Head is up (set to 0,-1,0 to look upside-down)
             );
  // Model matrix : an identity matrix (model will be at the origin)
  mModelMatrix.setToIdentity();
  mRotate.setToIdentity();

  shaderProgram.link();

  // Get a handle
  MatrixID = glGetUniformLocation(programID, "mMVP");
  glBindAttribLocation(programID, 0, "fragColor");

  // Create a Vertex Buffer Object
    glGenBuffers(1, &vertexbuffer);
  // Create an element array
    glGenBuffers(1, &elementbuffer);
}

bool GLwidget::DataLoad(QString fileName, bool first_time)
{

    int nV_cube = 48;
    int nE_cube = 36;
    GLfloat cube_vertices[] = {
                  -50.0f,  50.0f, -50.0f, 1.0f, 0.0f, 0.0f,  // Top-left & Red (x,y,z,r,g,b)
                  50.0f,  50.0f, -50.0f, 0.0f, 1.0f, 0.0f,  // Top-right & Green
                  50.0f, -50.0f, -50.0f, 0.0f, 0.0f, 1.0f,  // Bottom-right & Blue
                 -50.0f, -50.0f, -50.0f, 1.0f, 1.0f, 1.0f,   // Bottom-left & White
                  -50.0f,  50.0f, 50.0f, 1.0f, 1.0f, 0.0f,  // Top-left & Orange? (x,y,z,r,g,b)
                  50.0f,  50.0f, 50.0f, 0.0f, 1.0f, 1.0f,  // Top-right & Yellow?
                  50.0f, -50.0f, 50.0f, 1.0f, 0.0f, 1.0f,  // Bottom-right & Pink?
                 -50.0f, -50.0f, 50.0f, 0.0f, 0.0f, 0.0f   // Bottom-left & Black
              };


    // 12 triangles = 6 faces with triangles per face
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
    const char *filename = ba.data();
//    std::cout << "filename in C++ in DataLoad: " << filename << std::endl;
//    std::cout << "first_time: " << first_time << std::endl;

    if (!first_time)
     {
      // reload values to avoid seg fault if previous nV and nE are too small
      nV=34560;
      nE=26130;
      auto future1 = std::async([&]{return janus_(&flag, filename, elements, vertices, &nV, &nE);});
      future1.get();
      //      janus_(&flag, filename, elements, vertices, &nV, &nE);
     }
    else
     {
      nV=nV_cube;
      nE=nE_cube;
      //arrays have to be assigned this way vertices=cube_vertices only works in the same scope
      for (int i=0; i<= nV; ++i){
      vertices[i]=cube_vertices[i];
      }
      for (int i=0; i<= nE; ++i){
      elements[i]=cube_elements[i];
      }
     }

    std::cout << "DataLoad: nV: " << nV << std::endl;
    std::cout << "DataLoad vertices[6]: " << vertices[6] << std::endl;

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

    GLint posAttrib = glGetAttribLocation(programID, "position");
    glEnableVertexAttribArray(posAttrib);
    glVertexAttribPointer(posAttrib, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
                                                            // 6 floats = 3 positions + 3 colors per vertex
    // color attribute
    GLint colAttrib = glGetAttribLocation(programID, "incolor");
    glEnableVertexAttribArray(colAttrib);
    glVertexAttribPointer(colAttrib, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float)));
                                                           // offset 3 because colors start after 3 positions
    return true;
}

void GLwidget::paintGL(void)
{
    if ( !success ) return;  //not until shaders are built
    if ( !paintme ) return;  //not until nV, nE, vertices, elements are loaded
//     if ( !loaded ) return;  //not until nV, nE, vertices, elements are loaded
    if (!LoadSurfaceToBuffer(nV, nE, vertices, elements)) return;

    std::cout << "paint: nV: " << nV << std::endl;
    std::cout << "paint: vertices[6]: " << vertices[6] << std::endl;

    // Clear the screen
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    // Use our shader
    glUseProgram(programID);

 // Bind
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffer);

    // Send our transformation to the currently bound shader,
    // in the "MVP" uniform
    QMatrix4x4 MVP = mProjectionMatrix * mViewMatrix * mModelMatrix * mRotate;
    glUniformMatrix4fv(MatrixID, 1, GL_FALSE, MVP.data());

    glDrawElements(GL_TRIANGLES, nE, GL_UNSIGNED_INT, 0);

    // Unbind
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
  mWidth = w;
  mHeight = h;
  mProjectionMatrix.setToIdentity();
  mProjectionMatrix.perspective(45.0f, GLfloat(w) / h, 0.01f, 100.0f);
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
  rotate=false;
  if(e->button() == Qt::LeftButton)
  {
    oldX = e->position().toPoint().x();
    oldY = e->position().toPoint().y();
    newX = e->position().toPoint().x();
    newY = e->position().toPoint().y();
    rotate = true;
    useArcBall = true;
  }
}


void GLwidget::mouseMoveEvent(QMouseEvent *e)
{
  if(e->buttons() & Qt::LeftButton)
  {
    if(rotate)
    {
      newX = e->position().toPoint().x();
      newY = e->position().toPoint().y();
      updateMouse();
    }
      oldX = e->position().toPoint().x();
      oldY = e->position().toPoint().y();
  }
}


void GLwidget::mouseReleaseEvent(QMouseEvent *e)
{
  if(e->button() == Qt::LeftButton)
    useArcBall = false;
}

void GLwidget::updateMouse()
{
  QVector3D v = getArcBallVector(oldX,oldY); // from the mouse
  QVector3D u = getArcBallVector(newX, newY);

  float angle = std::acos(std::min(1.0f, QVector3D::dotProduct(u,v)));

  QVector3D rotAxis = QVector3D::crossProduct(v,u);
  QMatrix4x4 eye2ObjSpaceMat = mRotate.inverted();
  QVector3D objSpaceRotAxis = eye2ObjSpaceMat.map(rotAxis);

  oldX = newX;
  oldY = newY;

  mRotate.rotate(4 * qRadiansToDegrees(angle), objSpaceRotAxis);
  update();
}

QVector3D GLwidget::getArcBallVector(int x, int y)
{
   QVector3D pt = QVector3D(2.0 * x / mWidth - 1.0, 2.0 * y / mHeight  - 1.0 , 0);
   pt.setY(pt.y() * -1);

   // compute z-coordinates
   float xySquared = pt.x() * pt.x() + pt.y() * pt.y();

   if(xySquared <= 1.0)
       pt.setZ(std::sqrt(1.0 - xySquared));
   else
       pt.normalize();
   return pt;
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


