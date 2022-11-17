// adapted from https://github.com/QtOpenGL/qgl_tutorials

#include "kernunos.h"

// settings
const unsigned int SCR_WIDTH = 800;
const unsigned int SCR_HEIGHT = 600;

int flag=0;
int nV=34560;
int nE=26130;
std::vector<GLuint> Elements(nE);
std::vector<GLfloat> Vertices(nV);
GLfloat* vertices = Vertices.data();
GLuint* elements = Elements.data();

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

extern "C" {
void janus_(int *flag, const char *filename, GLuint *elements, GLfloat *vertices, int *nV, int *nE); // still needs an underscore despite c_interface.f90 bind C declaration
};

GLShaders::GLShaders()
{
    programID = 0;
}

GLShaders::~GLShaders()
{
    if ( programID )
        CleanUp();
}

void GLShaders::CleanUp()
{
    StopUse();

    glDeleteProgram(programID);

    glFlush();
}

GLuint GLShaders::GetAttribLoc(const std::string& name)
{
     return glGetAttribLocation(programID,name.c_str());
}


GLuint GLShaders::GetUnifLoc(const std::string& name)
{
    return glGetUniformLocation(programID, name.c_str());
}

void GLShaders::Init()
{
  initializeGL();
}

bool GLShaders::Use()
{

    glUseProgram(programID);
    return true;
}

void GLShaders::StopUse()
{
    glUseProgram(0);
}

GLTriangles::GLTriangles()
{
    vertexbuffer = elementbuffer = 0;
    m_triangShaders = NULL;
}

GLTriangles::~GLTriangles()
{
    Clear();
}

void GLTriangles::Clear()
{

    // Clear graphics card memory
 //   glBindBuffer(GL_ARRAY_BUFFER, 0);
 //   glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
    if ( elementbuffer )
        glDeleteBuffers(1, &elementbuffer);
    if ( vertexbuffer )
        glDeleteBuffers(1, &vertexbuffer);

 //   glFlush(); //Tell GL to execute those commands now, but we don't wait for them

    m_triangShaders = NULL;
    vertexbuffer = elementbuffer = 0;
}

void GLTriangles::SetBuffers(GLShaders* theShader,
                                int nV, int nE,
                                GLfloat* vertices, GLuint* elements)
{

    m_triangShaders = theShader;

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
    // position attribute
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);

    GLuint posAttrib = m_triangShaders->GetAttribLoc("position");
    glEnableVertexAttribArray(posAttrib);
    glVertexAttribPointer(posAttrib, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
                                                            // 6 floats = 3 positions + 3 colors per vertex

    // color attribute
    GLint colAttrib = m_triangShaders->GetAttribLoc("incolor");
    glEnableVertexAttribArray(colAttrib);
    glVertexAttribPointer(colAttrib, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float)));
                                                            // offset 3 because colors start after 3 positions
}

void GLTriangles::Draw()
{

    if ( ! m_triangShaders->Use() )
        return;

    paintGL();

}

GLManager::GLManager() {
   GLShaders m_TriangShaders;
   GLTriangles m_Triangles;
}

GLManager::~GLManager()
{
    glFinish();
}

const GLubyte* GLManager::GetGLVersion()
{
    return glGetString(GL_VERSION);
}

const GLubyte* GLManager::GetGLVendor()
{
    return glGetString(GL_VENDOR);
}

const GLubyte* GLManager::GetGLRenderer()
{
    return glGetString(GL_RENDERER);
}


void GLManager::SetShadersAndTriangles()
{
    m_TriangShaders.Init();
    m_Triangles.SetBuffers(&m_TriangShaders, nV, nE, vertices, elements);
}


void GLManager::Render()
{
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    glEnable(GL_DEBUG_OUTPUT);
    glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

    m_Triangles.Draw();

}

GLwidget::GLwidget (QWidget *parent ) : QOpenGLWidget(parent)

{
  setFocusPolicy(Qt::StrongFocus);
  cameraPos = QVector3D(0, 0, 6);

  // https://www.modernescpp.com/index.php/asynchronous-callable-wrappers
  static const unsigned int hwGuess= 4;
  //  these could come in handy later for available number of threads/cores for asynchronous tasks
      unsigned int hw = std::thread::hardware_concurrency();
      unsigned int hwConcurr= (hw != 0)? hw : hwGuess;
      std::cout << "Cores found by Kernunos: " << hwConcurr << std::endl;
}


GLwidget::~GLwidget()
{
  // cleanup
  makeCurrent();
  glDeleteBuffers(1, &vertexbuffer);
  glDeleteBuffers(1,&elementbuffer);
  glDeleteProgram(programID);
}

void GLwidget::initializeGL()
{
  // initialize OpenGL
  initializeOpenGLFunctions();

   m_oglManager = new GLManager();
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

  // Accept fragment if it closer to the camera than the former one
  glDepthFunc(GL_LESS);

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


}


void GLwidget::paintGL(void)
{
    if ( !success )
        return;

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

void GLwidget::timerEvent(QTimerEvent*)
{
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

void GLwidget::wheelEvent(QWheelEvent *e)
{
    QPoint numPixels = e->pixelDelta();

         if (numPixels.y() > 0) {
         mViewMatrix.translate(QVector3D(0,0,-0.1));
         update();
         }
         else {
         mViewMatrix.translate(QVector3D(0,0,0.1));
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

MainWindow::MainWindow()
{
    QWidget *widget = new QWidget;
    setCentralWidget(widget);

    QOpenGLWidget *topFiller = new GLwidget(this);
    topFiller->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    infoLabel = new QLabel(tr("<i>Welcome! Please Open a file.</i>"));
    infoLabel->setFrameStyle(QFrame::StyledPanel | QFrame::Sunken);
    infoLabel->setAlignment(Qt::AlignCenter);

//    QWidget *bottomFiller = new QWidget;
//    bottomFiller->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    QVBoxLayout *layout = new QVBoxLayout;
 //   layout->setContentsMargins(5, 5, 5, 5);
    layout->addWidget((topFiller));
    layout->addWidget(infoLabel);
//    layout->addWidget(bottomFiller);
    widget->setLayout(layout);

    createActions();
    createMenus();

    setWindowTitle(tr("GLwidget"));
    setMinimumSize(400, 400);
    resize(SCR_WIDTH, SCR_HEIGHT);
}

void MainWindow::open()
{
    infoLabel->setText(tr("Invoked <b>File|Open</b>"));
    QString fileName
        = QFileDialog::getOpenFileName(this, tr("Load a file"));
    if (fileName.isEmpty())
        return;
    QByteArray ba = fileName.toLocal8Bit();
    const char *filename = ba.data();
    std::cout << "filename in C++ " << filename << std::endl;
    if (!fileName.isEmpty())
    {

     }

    else {
/*        GLfloat vertices[] = {
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
            int nE = 36; */
        }
}


void MainWindow::save()
{
    infoLabel->setText(tr("Invoked <b>File|Save</b>"));
}

void MainWindow::print()
{
    infoLabel->setText(tr("Invoked <b>File|Print</b>"));
}

void MainWindow::about()
{
    infoLabel->setText(tr("Invoked <b>Help|About</b>"));
    QByteArray gla = m_GLString.toLocal8Bit();
    const char *glstring = gla.data();
    std::cout << glstring << std::endl;
    QMessageBox::about(this, tr("About Menu"),
            tr("The <b>Menu</b> example shows how to create "
               "menu-bar menus and context menus.\n",glstring));

}

void MainWindow::aboutQt()
{
    infoLabel->setText(tr("Invoked <b>Help|About Qt</b>"));
}


void MainWindow::createActions()
{

    openAct = new QAction(tr("&Open..."), this);
    openAct->setShortcuts(QKeySequence::Open);
    openAct->setStatusTip(tr("Open an existing file"));
    connect(openAct, &QAction::triggered, this, &MainWindow::open);

    saveAct = new QAction(tr("&Save"), this);
    saveAct->setShortcuts(QKeySequence::Save);
    saveAct->setStatusTip(tr("Save the document to disk"));
    connect(saveAct, &QAction::triggered, this, &MainWindow::save);

    printAct = new QAction(tr("&Print..."), this);
    printAct->setShortcuts(QKeySequence::Print);
    printAct->setStatusTip(tr("Print the document"));
    connect(printAct, &QAction::triggered, this, &MainWindow::print);

    exitAct = new QAction(tr("E&xit"), this);
    exitAct->setShortcuts(QKeySequence::Quit);
    exitAct->setStatusTip(tr("Exit the application"));
    connect(exitAct, &QAction::triggered, this, &QWidget::close);

    aboutAct = new QAction(tr("&About"), this);
    aboutAct->setStatusTip(tr("Show the application's About box"));
    connect(aboutAct, &QAction::triggered, this, &MainWindow::about);

    aboutQtAct = new QAction(tr("About &Qt"), this);
    aboutQtAct->setStatusTip(tr("Show the Qt library's About box"));
    connect(aboutQtAct, &QAction::triggered, qApp, &QApplication::aboutQt);
    connect(aboutQtAct, &QAction::triggered, this, &MainWindow::aboutQt);

}

void MainWindow::createMenus()
{
    fileMenu = menuBar()->addMenu(tr("&File"));
    fileMenu->addAction(openAct);
    fileMenu->addAction(saveAct);
    fileMenu->addAction(printAct);
    fileMenu->addSeparator();
    fileMenu->addAction(exitAct);
    helpMenu = menuBar()->addMenu(tr("&Help"));
    helpMenu->addAction(aboutAct);
    helpMenu->addAction(aboutQtAct);


}


int main(int argc, char *argv[])
{
  //  Q_INIT_RESOURCE(GLwidget);

    QApplication app(argc, argv);
    QCoreApplication::setOrganizationName("QtProject");
    QCoreApplication::setApplicationName("Application Example");
    QCoreApplication::setApplicationVersion(QT_VERSION_STR);
    QCommandLineParser parser;
    parser.setApplicationDescription(QCoreApplication::applicationName());
    parser.addHelpOption();
    parser.addVersionOption();
    parser.addPositionalArgument("file", "The file to open.");
    parser.process(app);

    QTranslator translator;
    const QStringList uiLanguages = QLocale::system().uiLanguages();
    for (const QString &locale : uiLanguages) {
        const QString baseName = "untitled_" + QLocale(locale).name();
        if (translator.load(":/i18n/" + baseName)) {
            app.installTranslator(&translator);
            break;
        }
    }

   MainWindow window;
//    GLwidget window;
    window.show();
    return app.exec();
/*
    MainWindow mainWin;
    if (!parser.positionalArguments().isEmpty())
        mainWin.loadFile(parser.positionalArguments().first());
    mainWin.show();
*/
    return app.exec();
}

