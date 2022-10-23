// adapted from https://github.com/QtOpenGL/qgl_tutorials
#include <cmath>
#include <QtMath>

// Include standard headers
#include <QtWidgets>
#include <QApplication>
#include <stdio.h>
#include <chrono>
#include <iostream>
#include <future>
#include <thread>
#include <memory>
#include <cmath>
#include "kernunos.h"

// Our vertices. Tree consecutive floats give a 3D vertex; Three consecutive vertices give a triangle.
// A cube has 6 faces with 2 triangles each, so this makes 6*2=12 triangles, and 12*3 vertices
static const float g_vertex_buffer_data[] = {
  -1.0f,-1.0f,-1.0f,
  -1.0f,-1.0f, 1.0f,
  -1.0f, 1.0f, 1.0f,
   1.0f, 1.0f,-1.0f,
  -1.0f,-1.0f,-1.0f,
  -1.0f, 1.0f,-1.0f,
   1.0f,-1.0f, 1.0f,
  -1.0f,-1.0f,-1.0f,
   1.0f,-1.0f,-1.0f,
   1.0f, 1.0f,-1.0f,
   1.0f,-1.0f,-1.0f,
  -1.0f,-1.0f,-1.0f,
  -1.0f,-1.0f,-1.0f,
  -1.0f, 1.0f, 1.0f,
  -1.0f, 1.0f,-1.0f,
   1.0f,-1.0f, 1.0f,
  -1.0f,-1.0f, 1.0f,
  -1.0f,-1.0f,-1.0f,
  -1.0f, 1.0f, 1.0f,
  -1.0f,-1.0f, 1.0f,
   1.0f,-1.0f, 1.0f,
   1.0f, 1.0f, 1.0f,
   1.0f,-1.0f,-1.0f,
   1.0f, 1.0f,-1.0f,
   1.0f,-1.0f,-1.0f,
   1.0f, 1.0f, 1.0f,
   1.0f,-1.0f, 1.0f,
   1.0f, 1.0f, 1.0f,
   1.0f, 1.0f,-1.0f,
  -1.0f, 1.0f,-1.0f,
   1.0f, 1.0f, 1.0f,
  -1.0f, 1.0f,-1.0f,
  -1.0f, 1.0f, 1.0f,
   1.0f, 1.0f, 1.0f,
  -1.0f, 1.0f, 1.0f,
   1.0f,-1.0f, 1.0f
};

/*
    GLfloat vertices[] = {                                 // modded to 3D
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



// Two UV coordinatesfor each vertex. They were created withe Blender.
static const float g_uv_buffer_data[] = {
  0.000059f, 1.0f-0.000004f,
  0.000103f, 1.0f-0.336048f,
  0.335973f, 1.0f-0.335903f,
  1.000023f, 1.0f-0.000013f,
  0.667979f, 1.0f-0.335851f,
  0.999958f, 1.0f-0.336064f,
  0.667979f, 1.0f-0.335851f,
  0.336024f, 1.0f-0.671877f,
  0.667969f, 1.0f-0.671889f,
  1.000023f, 1.0f-0.000013f,
  0.668104f, 1.0f-0.000013f,
  0.667979f, 1.0f-0.335851f,
  0.000059f, 1.0f-0.000004f,
  0.335973f, 1.0f-0.335903f,
  0.336098f, 1.0f-0.000071f,
  0.667979f, 1.0f-0.335851f,
  0.335973f, 1.0f-0.335903f,
  0.336024f, 1.0f-0.671877f,
  1.000004f, 1.0f-0.671847f,
  0.999958f, 1.0f-0.336064f,
  0.667979f, 1.0f-0.335851f,
  0.668104f, 1.0f-0.000013f,
  0.335973f, 1.0f-0.335903f,
  0.667979f, 1.0f-0.335851f,
  0.335973f, 1.0f-0.335903f,
  0.668104f, 1.0f-0.000013f,
  0.336098f, 1.0f-0.000071f,
  0.000103f, 1.0f-0.336048f,
  0.000004f, 1.0f-0.671870f,
  0.336024f, 1.0f-0.671877f,
  0.000103f, 1.0f-0.336048f,
  0.336024f, 1.0f-0.671877f,
  0.335973f, 1.0f-0.335903f,
  0.667969f, 1.0f-0.671889f,
  1.000004f, 1.0f-0.671847f,
  0.667979f, 1.0f-0.335851f
};

const GLchar* vertexSource = R"glsl(
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

const GLchar* fragmentSource = R"glsl(
#version 400 core
out vec4 fragColor;
in vec3 outColor;

void main()
{
    fragColor = vec4(outColor, 1.0);
}
)glsl";

const GLchar* TransformFragmentShader = R"glsl(
#version 120

// Interpolated values from the vertex shaders
varying vec2 UV;

// Ouput data
//out vec3 color;

// Values that stay constant for the whole mesh.
uniform sampler2D myTextureSampler;

void main(){

        // Output color = color of the texture at the specified UV
        gl_FragColor = texture2D( myTextureSampler, UV );
}
)glsl";

const GLchar* TransformVertexShader = R"glsl(
#version 120

// Input vertex data, different for all executions of this shader.
attribute vec3 vertexPosition_modelspace;
attribute vec2 vertexUV;

// Output data ; will be interpolated for each fragment.
varying vec2 UV;

// Values that stay constant for the whole mesh.
uniform mat4 MVP;

void main(){

    // Output position of the vertex, in clip space : MVP * position
    gl_Position =  MVP * vec4(vertexPosition_modelspace,1);

    // UV of the vertex. No special space for this one.
    UV = vertexUV;
}
)glsl";


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
  glDeleteBuffers(1, &uvbuffer);
  glDeleteProgram(programID);
  delete mTexture;
}

void kernunos::initializeGL()
{
  // initialize OpenGL
  initializeOpenGLFunctions();

  // Dark blue background
  glClearColor(0.0f, 0.0f, 0.3f, 0.0f);

  // Enable depth test
  glEnable(GL_DEPTH_TEST);

  // Accept fragment if it closer to the camera than the former one
  glDepthFunc(GL_LESS);

  bool success;
  // load and compile vertex shader
  success = shaderProgram.addShaderFromSourceCode(QOpenGLShader::Vertex,TransformVertexShader);
  if (success) std::cout << "compiled vertex" << std::endl;
  if (!success) std::cout << "DID NOT compile vertex" << std::endl;

  // load and compile fragment shader
  success = shaderProgram.addShaderFromSourceCode(QOpenGLShader::Fragment,TransformFragmentShader);
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

  // Get a handle for our "MVP" uniform
  MatrixID = glGetUniformLocation(programID, "MVP");

  // Get a handle for our buffers
  vertexPosition_modelspaceID = glGetAttribLocation(programID, "vertexPosition_modelspace");
  vertexUVID = glGetAttribLocation(programID, "vertexUV");

  //enable texturing
  glEnable(GL_TEXTURE_2D);

  // Load the texture Qt methods
  mTexture = new QOpenGLTexture( QImage(":/uvtemplate.bmp").mirrored() );
  mTexture->setWrapMode(QOpenGLTexture::Repeat);
  mTexture->setMinificationFilter(QOpenGLTexture::LinearMipMapLinear);
  mTexture->setMagnificationFilter(QOpenGLTexture::Linear);
  shaderProgram.setUniformValue("myTextureSampler", 0);

  glGenBuffers(1, &vertexbuffer);
  glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);

  glGenBuffers(1, &uvbuffer);
  glBindBuffer(GL_ARRAY_BUFFER, uvbuffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(g_uv_buffer_data), g_uv_buffer_data, GL_STATIC_DRAW);

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

    // Bind our texture in Texture Unit 0
    mTexture->bind(0);

    // 1rst attribute buffer : vertices
    glEnableVertexAttribArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    glVertexAttribPointer(
      vertexPosition_modelspaceID,  // The attribute we want to configure
      3,                            // size
      GL_FLOAT,                     // type
      GL_FALSE,                     // normalized?
      0,                            // stride
      (void*)0                      // array buffer offset
    );

    // 2nd attribute buffer : UVs
    glEnableVertexAttribArray(1);
    glBindBuffer(GL_ARRAY_BUFFER, uvbuffer);
    glVertexAttribPointer(
      vertexUVID,                   // The attribute we want to configure
      2,                            // size : U+V => 2
      GL_FLOAT,                     // type
      GL_FALSE,                     // normalized?
      0,                            // stride
      (void*)0                      // array buffer offset
    );

    // Draw the triangles !
    glDrawArrays(GL_TRIANGLES, 0, 12*3); // From index 0 to 12*3 -> 12 triangles

    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);

}

void kernunos::timerEvent(QTimerEvent*)
{
}

void kernunos::resizeGL(int w, int h)
{
  mWidth = w;
  mHeight = h;
  glViewport(0, 0, (GLsizei) w, (GLsizei) h);
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
    oldX = e->x(); // Set this to the mouse position
    oldY = e->y(); // Set this to the mouse position

    newX = e->x();
    newY = e->y();

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
      newX = e->x();
      newY = e->y();
      updateMouse();

    }
    oldX = e->x();
    oldY = e->y();
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

        //oldRot = newRot;

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



