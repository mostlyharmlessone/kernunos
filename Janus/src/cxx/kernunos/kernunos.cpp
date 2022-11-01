// adapted from https://github.com/QtOpenGL/qgl_tutorials

#include "kernunos.h"

// need to allocate real values here; can't wait for file read
int flag=0;
int nV=34560;
int nE=26130;
std::vector<GLuint> Elements(nE);
std::vector<GLfloat> Vertices(nV);
GLfloat* vertices = Vertices.data();
GLuint* elements = Elements.data();

kernunos::kernunos( QWidget *parent ) : QOpenGLWidget(parent)
{
  cameraPos = QVector3D(0, 0, 6);


  // https://www.modernescpp.com/index.php/asynchronous-callable-wrappers
  static const unsigned int hwGuess= 4;
  //  these could come in handy later for available number of threads/cores for asynchronous tasks
      unsigned int hw = std::thread::hardware_concurrency();
      unsigned int hwConcurr= (hw != 0)? hw : hwGuess;
      std::cout << "Cores found by Jupiter: " << hwConcurr << std::endl;
}

kernunos::~kernunos()
{
  // Cleanup VBO and shader
  makeCurrent();
  glDeleteBuffers(1, &vertexbuffer);
  glDeleteBuffers(1, &arraybuffer);
  glDeleteBuffers(1,&elementbuffer);
  glDeleteProgram(programID);
}

/*
    GLfloat vertices[] = {
        -0.5f,  0.5f, -0.5f, 1.0f, 0.0f, 0.0f,  // Top-left & Red (x,y,z,r,g,b)
        0.5f,  0.5f, -0.5f, 0.0f, 1.0f, 0.0f,  // Top-right & Green
        0.5f, -0.5f, -0.5f, 0.0f, 0.0f, 1.0f,  // Bottom-right & Blue
       -0.5f, -0.5f, -0.5f, 1.0f, 1.0f, 1.0f,   // Bottom-left & White
        -0.5f,  0.5f, 0.5f, 1.0f, 1.0f, 0.0f,  // Top-left & Orange? (x,y,z,r,g,b)
        0.5f,  0.5f, 0.5f, 0.0f, 1.0f, 1.0f,  // Top-right & Yellow?
        0.5f, -0.5f, 0.5f, 1.0f, 0.0f, 1.0f,  // Bottom-right & Pink?
       -0.5f, -0.5f, 0.5f, 0.0f, 0.0f, 0.0f   // Bottom-left & Black
    };
    int nV = 48;

    GLuint elements[] = {  // 12 triangles = 6 faces with triangles per face
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
    int nE = 36;
*/

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



void kernunos::initializeGL()
{
  // initialize OpenGL
  initializeOpenGLFunctions();

  glClearColor(0.2f, 0.3f, 0.3f, 0.0f);

  // Enable depth test
  glEnable(GL_DEPTH_TEST);

  // Accept fragment if it closer to the camera than the former one
  glDepthFunc(GL_LESS);

  bool success;

  // load and compile vertex shader
  success = shaderProgram.addShaderFromSourceCode(QOpenGLShader::Vertex,vertexSource);
  if (success) std::cout << "compiled vertex" << std::endl;
  if (!success) std::cout << "DID NOT compile vertex" << std::endl;

  // load and compile fragment shader
  success = shaderProgram.addShaderFromSourceCode(QOpenGLShader::Fragment,fragmentSource);
  if (success) std::cout << "compiled frag" << std::endl;
  if (!success) std::cout << "DID NOT compile frag" << std::endl;
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

  glGenBuffers(1, &arraybuffer);
  glBindBuffer(GL_ARRAY_BUFFER, arraybuffer);

  // Create a Vertex Buffer Object and copy the vertex data to it
  glGenBuffers(1, &vertexbuffer);  //generate 1 buffer

  glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
  GLint data_size_in_bytes = sizeof(GLfloat)*nV;                              // 4-bytes per float x number of vertices
  glBufferData(GL_ARRAY_BUFFER, data_size_in_bytes, vertices, GL_STATIC_DRAW);

  GLint size = 0;
  glGetBufferParameteriv(GL_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);
   if(data_size_in_bytes != size)
    {
     glDeleteBuffers(1, &vertexbuffer);
     std::cout << "Error in vertices buffer: " << data_size_in_bytes << ", " << size << std::endl;
     return;
    }

  // Create an element array
   glGenBuffers(1, &elementbuffer);
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffer);
   data_size_in_bytes = sizeof(GLuint)*nE;                // 4-bytes per int x number of elements
   glBufferData(GL_ELEMENT_ARRAY_BUFFER, data_size_in_bytes, elements, GL_STATIC_DRAW);

   size = 0;
   glGetBufferParameteriv(GL_ELEMENT_ARRAY_BUFFER, GL_BUFFER_SIZE, &size);
    if(data_size_in_bytes != size)
      {
       glDeleteBuffers(1, &elementbuffer);
       std::cout << "Error in element buffer: " << data_size_in_bytes << ", " << size << std::endl;
       return;
      }

   // Unbind
   glBindBuffer(GL_ARRAY_BUFFER, 0);
   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

}

void kernunos::paintGL(void)
{
    // Clear the screen
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // Use our shader
    glUseProgram(programID);

    // Send our transformation to the currently bound shader,
    // in the "MVP" uniform
    QMatrix4x4 MVP = mProjectionMatrix * mViewMatrix * mModelMatrix * mRotate;
    glUniformMatrix4fv(MatrixID, 1, GL_FALSE, MVP.data());

    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, elementbuffer);

    GLint posAttrib = glGetAttribLocation(programID, "position");
    glEnableVertexAttribArray(posAttrib);
    glVertexAttribPointer(posAttrib, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
                                                            // 6 floats = 3 positions + 3 colors per vertex
    // color attribute
    GLint colAttrib = glGetAttribLocation(programID, "incolor");
    glEnableVertexAttribArray(colAttrib);
    glVertexAttribPointer(colAttrib, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float)));
                                                           // offset 3 because colors start after 3 positions

    glDrawElements(GL_TRIANGLES, nE, GL_UNSIGNED_INT, 0);

   // Unbind

    // Unbind
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

}

void kernunos::timerEvent(QTimerEvent*)
{
}

void kernunos::resizeGL(int w, int h)
{
  mWidth = w;
  mHeight = h;
  mProjectionMatrix.setToIdentity();
  mProjectionMatrix.perspective(45.0f, GLfloat(w) / h, 0.01f, 100.0f);
  update();
}

void kernunos::keyPressEvent(QKeyEvent *e)
{
  switch (e->key())
  {
    case Qt::Key_Escape:  /*  Escape Key */
     exit(0);
      break;
    case Qt::Key_Q:  /*  Q Key */
     mViewMatrix.translate(QVector3D(0,0,-0.1));
     update();
      break;
  case Qt::Key_S:  /*  S Key */
     mViewMatrix.translate(QVector3D(0,0,0.1));
     update();
      break;
  case Qt::Key_W:  /*  W Key */
     mViewMatrix.translate(QVector3D(0,0.1,0));
     update();
      break;
  case Qt::Key_X:  /*  X Key */
     mViewMatrix.translate(QVector3D(0,-0.1,0));
     update();
      break;
  case Qt::Key_A:  /*  A Key */
     mViewMatrix.translate(QVector3D(-0.1,0,0));
     update();
      break;
  case Qt::Key_D:  /*  D Key */
     mViewMatrix.translate(QVector3D(0.1,0,0));
     update();
      break;
    default:
      break;
  }
  e->accept();  // Don't pass any key events to parent
}

void kernunos::wheelEvent(QWheelEvent * event)
{
#if 0
  m_distExp += event->delta();
  if (m_distExp < -8 * 120)
    m_distExp = -8 * 120;
  if (m_distExp > 10 * 120)
    m_distExp = 10 * 120;
  event->accept();
#endif
}

void kernunos::mousePressEvent(QMouseEvent *e)
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


void kernunos::mouseMoveEvent(QMouseEvent *e)
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


void kernunos::mouseReleaseEvent(QMouseEvent *e)
{
  if(e->button() == Qt::LeftButton)
    useArcBall = false;
}

void kernunos::updateMouse()
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


QVector3D kernunos::getArcBallVector(int x, int y)
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



