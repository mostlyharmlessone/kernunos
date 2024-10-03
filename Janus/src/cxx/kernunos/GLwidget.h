#ifndef GLWIDGET_H
#define GLWIDGET_H

// Include standard headers
#include <QtWidgets>
#include <QApplication>
#include <QMouseEvent>
#include <QCoreApplication>
#include <QMainWindow>
#include <QKeySequence>
#include <QMenuBar>
#include <QMenu>
#include <QMessageBox>

#include <cmath>
#include <math.h>
#include <stdio.h>
#include <chrono>
#include <iostream>
#include <future>
#include <thread>
#include <vector>
#include <memory>
#include <functional>

#include <QOpenGLWidget>
#include <QOpenGLShaderProgram>
#include <QOpenGLFunctions>
#include <QOpenGLTexture>
#include <QKeyEvent>
#include <QTime>
#include <QtMath>
#include <QVector3D>
#include <QMatrix4x4>
#include <QPointF>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLBuffer>

#include <QCommandLineParser>
#include <QCommandLineOption>
#include <QLocale>
#include <QTranslator>

#include <qt6/QtCore/qtmetamacros.h>

// global variables
extern const unsigned int SCR_WIDTH;
extern const unsigned int SCR_HEIGHT;

extern int flag;

extern bool success;
extern bool paintme;

//how very FORTRAN that these need to be static & global
extern int nV;
extern int nE;
extern std::vector<GLuint> Elements;
extern std::vector<GLfloat> Vertices;
extern GLfloat* vertices;
extern GLuint* elements;
extern std::vector<GLuint> Elements2;
extern std::vector<GLfloat> Vertices2;
extern GLfloat* vertices2;
extern GLuint* elements2;

extern std::vector<GLuint> Elements3;
extern std::vector<GLfloat> Vertices3;
extern GLfloat* vertices3;
extern GLuint* elements3;

extern int pupil_nV;
extern int pupil_nE;
extern std::vector<GLuint> pupil_Elements;
extern std::vector<GLfloat> pupil_Vertices;
extern GLfloat* pupil_vertices;
extern GLuint* pupil_elements;

extern "C" {
void janus_(int *flag,char *filename,GLuint *elements,GLfloat *vertices,float *legend,float *zern,int *nV,int *nE,int *nL,int *nZ,GLuint *pupil_elements,GLfloat *pupil_vertices,int *pupil_nV, int *pupil_nE);
// needs an underscore despite c_interface.f90 bind C declaration
};

extern QString *m_GLString;
extern QString glstring_global;


QT_BEGIN_NAMESPACE
class QAction;
class QActionGroup;
class QLabel;
class QMenu;
QT_END_NAMESPACE

class MainWindow ;

QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)

class GLwidget : public QOpenGLWidget, protected QOpenGLFunctions
{
    Q_OBJECT

  public:
    GLwidget( QWidget *parent = nullptr );
    ~GLwidget();

//  These are all independent boolean choices
    static bool isTransparent() { return m_transparent; }
    static void setTransparent(bool t) { m_transparent = t; }

    static bool isNormal() { return m_normal; }
    static void setNormal(bool t) { m_normal = t; }

    static bool isLight() { return m_lighting; }
    static void setLight(bool t) { m_lighting = t; }

    static bool isPupil() { return m_pupilshow; }
    static void setPupil(bool t) { m_pupilshow = t; }

    static bool ispupilregister() { return m_pupilregister; }
    static void setpupilregister(bool t) { m_pupilregister = t; }

    static bool isRedraw() { return m_redraw; }
    static void setRedraw(bool t) { m_redraw = t; }

    static bool isdecenter() { return m_decenter; }
    static void setdecenter(bool t) { m_decenter = t; }

    static bool isCenterNode() { return m_centerNode; }
    static void setCenterNode(bool t) { m_centerNode = t;
      int dat=(flag-(flag%1000000))/1000000;
      if(t) {dat |= 1UL << 0;} else { dat &= ~(1UL << 0);}  //set/unset 0th ie. first bit
      // m_centerNode =(dat >> 0) & 1U;  should agree with m_centerNode = t;
      flag=1000000*dat+(flag%1000000); }

    static bool isadjustradii() { return m_adjustradii; }
    static void setadjustradii(bool t) { m_adjustradii = t;
      int dat=(flag-(flag%1000000))/1000000;
      if(t) {dat |= 1UL << 1;} else { dat &= ~(1UL << 1);}  //set/unset 1st ie. second bit
      // m_adjustradii =(dat >> 1) & 1U;  should agree with m_adjustradii = t;
      flag=1000000*dat+(flag%1000000); }

    static bool iscubic() { return m_cubic; }   //default is trapez
    static void setcubic(bool t) { m_cubic = t;
      int dat=(flag-(flag%1000000))/1000000;
      if(t) {dat |= 1UL << 2;} else { dat &= ~(1UL << 2);}  //set/unset 2st ie. third bit
      // m_adjustradii =(dat >> 2) & 1U;  should agree with m_cubic = t;
      flag=1000000*dat+(flag%1000000); }

    // these two are mutually exclusive but can both be false

    static bool isLSQfillin() { return m_LSQfillin; }
    static void setLSQfillin(bool t) {
      int dat=(flag-(flag%1000000))/1000000;
      if(t) {dat |= 1UL << 3;
             m_LSQfillin = true;
             dat &= ~(1UL << 4);
             m_Splinefillin = false;}
      else { dat &= ~(1UL << 3);
             m_LSQfillin = false;}   //set/unset 3nd ie. fourth bit
      // m_LSQfillin =(dat >> 3) & 1U;  should agree with m_LSQfillin = t;
      flag=1000000*dat+(flag%1000000); }

    static bool isSplinefillin() { return m_Splinefillin; }
    static void setSplinefillin(bool t) {
      int dat=(flag-(flag%1000000))/1000000;
      if(t) {dat |= 1UL << 4;
             m_Splinefillin = true;
             dat &= ~(1UL << 3);
             m_LSQfillin = false;}
      else { dat &= ~(1UL << 4);
             m_Splinefillin = false;}  //set/unset 4rd ie. fifth bit
      // m_adjustradii =(dat >> 4) & 1U;  should agree with m_Splinefillin = t;
      flag=1000000*dat+(flag%1000000); }

    static void setAllfctfalse(){
      m_Axial = false;
      m_Oblique = false;
      m_Tangential = false;
      m_Instantaneous = false;
      m_Mean = false;
      m_MongeAstig = false;
      m_Elevation = false;
      Z44VerticalQuatrafoil = false;
      Z42Vertical2ndAstig = false;
      Z40SphericalAberration = false;
      Z4neg2Oblique2ndAstig = false;
      Z4neg4ObliqueQuatrafoil = false;
      Z33ObliqueTrefoil = false;
      Z3neg3VerticalTrefoil = false;
      Z31HorizontalComa = false;
      Z3neg1VerticalComa = false;
      Z22VerticalAstig = false;
      Z2neg2ObliqueAstig = false;
      Z20Defocus = false;
      Z11Xtilt = false;
      Z1neg1Ytilt = false;
      Z00Piston = false;}

    static void setAllmapsfalse(){
      m_rgb2 = false;
      m_rgb5 = false;
      m_hsbrgb = false;
      m_gplotpalette = false;
      m_USSfixed = false;
      m_perceptualuniformfixed = false;
      m_USSpalette = false;
      m_perceptualuniformpalette = false;
    }

//  These are all mutually exclusive choices, so picking one clears the rest
    static bool isAxial() { return m_Axial; }
    static void setAxial(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(0-fct); // 0 sets to Axial
      setAllfctfalse();
      m_Axial = t;}

    static bool isOblique() { return m_Oblique; }
    static void setOblique(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(21-fct); // 21 sets to Oblique
      setAllfctfalse();
      m_Oblique = t;}

    static bool isTangential() { return m_Tangential; }
    static void setTangential(bool t) { 
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(16-fct); // 16 sets to Tangential
      setAllfctfalse();
      m_Tangential = t;}

    static bool isInstantaneous() { return m_Instantaneous; }
    static void setInstantaneous(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(17-fct); // 17 sets to Tangential
      setAllfctfalse();
      m_Instantaneous = t;}

    static bool isMean() { return m_Mean; }
    static void setMean(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(18-fct); // 18 sets to Mean
      setAllfctfalse();
      m_Mean = t;}

    static bool isAstig() { return m_MongeAstig; }
    static void setAstig(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(19-fct); // 19 sets to MongeAstig
      setAllfctfalse();
      m_MongeAstig = t;}

    static bool isElevation() { return m_Elevation; }
    static void setElevation(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(20-fct); // 20 sets to Elevation
      setAllfctfalse();
      m_Elevation = t;}

    static bool isZ44() { return Z44VerticalQuatrafoil; }
    static void setZ44(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(15-fct); // 15 sets to VerticalQuad
      setAllfctfalse();
      Z44VerticalQuatrafoil = t;}

    static bool isZ42() { return Z42Vertical2ndAstig; }
    static void setZ42(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(13-fct); // 13 sets to VerticalQuad
      setAllfctfalse();
      Z42Vertical2ndAstig = t;}

    static bool isZ40() { return Z40SphericalAberration; }
    static void setZ40(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(13-fct); // 13 sets to VerticalQuad
      setAllfctfalse();
      Z40SphericalAberration = t;}

    static bool isZ4neg2() { return Z4neg2Oblique2ndAstig; }
    static void setZ4neg2(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(4-fct); // 4 sets to Oblique2ndAstig
      setAllfctfalse();
      Z4neg2Oblique2ndAstig = t;}

    static bool isZ4neg4() { return Z4neg4ObliqueQuatrafoil; }
    static void setZ4neg4(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(1-fct); // 1 sets to ObliqueQuatrafoil
      setAllfctfalse();
      Z4neg4ObliqueQuatrafoil = t;}

    static bool isZ33() { return Z33ObliqueTrefoil; }
    static void setZ33(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(14-fct); // 14 sets to ObliqueTrefoil
      setAllfctfalse();
      Z33ObliqueTrefoil = t;}

    static bool isZ31() { return Z31HorizontalComa; }
    static void setZ31(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(11-fct); // 11 sets to HorizontalComa
      setAllfctfalse();
      Z31HorizontalComa = t;}

    static bool isZ3neg1() { return Z3neg1VerticalComa; }
    static void setZ3neg1(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(6-fct); // 6 sets to VerticalComa
      setAllfctfalse();
      Z3neg1VerticalComa = t;}

    static bool isZ3neg3() { return Z3neg3VerticalTrefoil; }
    static void setZ3neg3(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(2-fct); // 2 sets to VerticalTrefoil
      setAllfctfalse();
      Z3neg3VerticalTrefoil = t;}

    static bool isZ22() { return Z22VerticalAstig; }
    static void setZ22(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(12-fct); // 12 sets to VerticalAstig
      setAllfctfalse();
      Z22VerticalAstig = t;}

    static bool isZ20() { return Z20Defocus; }
    static void setZ20(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(8-fct); // 8 sets to Defocus
      setAllfctfalse();
      Z20Defocus = t;}

    static bool isZ2neg2() { return Z2neg2ObliqueAstig; }
    static void setZ2neg2(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(3-fct); // 3 sets to ObliqueAstig
      setAllfctfalse();
      Z2neg2ObliqueAstig = t;}

    static bool isZ11() { return Z11Xtilt; }
    static void setZ11(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(10-fct); // 10 sets to X-tilt
      setAllfctfalse();
      Z11Xtilt = t;}

    static bool isZ1neg1() { return Z1neg1Ytilt; }
    static void setZ1neg1(bool t) {
      int fct=((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(5-fct); // 5 sets to Y-tilt
      setAllfctfalse();
      Z1neg1Ytilt = t;}

    static bool isPiston() { return Z00Piston; }
    static void setPiston(bool t) {
      int fct= ((flag-(flag%10000))/10000)%100 ;
      flag=flag+10000*(7-fct); // 7 sets to Piston
      setAllfctfalse();
      Z00Piston = t;}

    static bool isrgb2() { return m_rgb2; }
    static void setrgb2(bool t) {
      int map=(flag-(flag%100))/100%100 ;
      flag=flag+100*(1-map) ; // 1 sets palette to rgb2
      setAllmapsfalse();
      m_rgb2 = t;}

    static bool isrgb5() { return m_rgb5; }
    static void setrgb5(bool t) {
      int map=(flag-(flag%100))/100%100 ;
      flag=flag+100*(2-map) ; // 2 sets palette to rgb5
      setAllmapsfalse();
      m_rgb5 = t;}

    static bool ishsbrgb() { return m_hsbrgb; }
    static void sethsbrgb(bool t) {
      int map=(flag-(flag%100))/100%100 ;
      flag=flag+100*(3-map) ; // 3 sets palette to hsbrgb
      setAllmapsfalse();
      m_hsbrgb = t;}

    static bool isgplotpalette() { return m_gplotpalette; }
    static void setgplotpalette(bool t) {
      int map=(flag-(flag%100))/100%100 ;
      flag=flag+100*(4-map) ; // 4 sets palette to gplotpalette
      setAllmapsfalse();
      m_gplotpalette = t;}

    static bool isUSSfixed() { return m_USSfixed; }
    static void setUSSfixed(bool t) {
      int map=(flag-(flag%100))/100%100 ;
      flag=flag+100*(5-map) ; // 5 sets palette to USS palette with fixed range (default)
      setAllmapsfalse();
      m_USSfixed = t;}

    static bool isperceptualuniformfixed() { return m_perceptualuniformfixed; }
    static void setperceptualuniformfixed(bool t) {
      int map=(flag-(flag%100))/100%100 ;
      flag=flag+100*(6-map) ; // 6 sets palette to Perceptually uniform palette with fixed range
      setAllmapsfalse();
      m_perceptualuniformfixed = t;}

    static bool isUSSpalette() { return m_USSpalette; }
    static void setUSSpalette(bool t) {
      int map=(flag-(flag%100))/100%100 ;
      flag=flag+100*(7-map) ; // 7 sets palette to USS palette
      setAllmapsfalse();
      m_USSpalette = t;}

    static bool isperceptualuniformpalette() { return m_perceptualuniformpalette; }
    static void setperceptualuniformpalette(bool t) {
      int map=(flag-(flag%100))/100%100 ;
      flag=flag+100*(8-map) ; // 8 sets palette to Perceptually uniform palette
      setAllmapsfalse();
      m_perceptualuniformpalette = t;}


    QSize minimumSizeHint() const override;
    QSize sizeHint() const override;

    bool DataLoad(QString fileName, bool filepresent);
    bool Swap();

  public slots:

    void setXRotation(int angle);
    void setYRotation(int angle);
    void setZRotation(int angle);
    void cleanup();


  signals:

    void xRotationChanged(int angle);
    void yRotationChanged(int angle);
    void zRotationChanged(int angle);


  protected:

    void initializeGL(void) override;
    void resizeGL( int w, int h ) override;
    void paintGL() override;
    void keyPressEvent( QKeyEvent *e) override;
    void mouseMoveEvent(QMouseEvent *e) override;
    void mousePressEvent(QMouseEvent *e) override;
    void wheelEvent(QWheelEvent *e) override;
    void timerEvent(QTimerEvent*) override;

  private:

    MainWindow *m_parent;
    QOpenGLShaderProgram *shaderProgram = nullptr;
    QOpenGLShaderProgram *shaderGeoProgram = nullptr;
    QOpenGLShaderProgram *shaderNormalProgram = nullptr;
    QMatrix4x4 projectionMatrix;
    QMatrix4x4 mViewMatrix;
    bool LoadSurfaceToBuffer(int nV, int nE, GLuint vertexbuffer,  GLuint elementbuffer, GLfloat *vertices, GLuint *elements);

    GLuint elementbuffers[4];
    GLuint vertexbuffers[4];
    int timerID;

    void setupVertexAttribs();

    int m_xRot = 0;
    int m_yRot = 0;
    int m_zRot = 0;
    QPoint m_lastPos;
    QOpenGLVertexArrayObject m_vao;
    int m_projMatrixLoc = 0;
    int m_viewMatrixLoc = 0;
    int m_lightPosLoc = 0;
    int m_alphaLoc = 0;
    QMatrix4x4 m_camera;
    QMatrix4x4 m_world;
    static bool m_transparent;
    static bool m_normal;
    static bool m_lighting;
    static bool m_pupilshow;
    static bool m_redraw;

    static bool m_centerNode;
    static bool m_adjustradii;
    static bool m_cubic;
    static bool m_LSQfillin;
    static bool m_Splinefillin;
    static bool m_decenter;
    static bool m_pupilregister;

    static bool m_Axial;
    static bool m_Oblique;
    static bool m_Tangential;
    static bool m_Instantaneous;
    static bool m_Mean;
    static bool m_MongeAstig;
    static bool m_Elevation;
    static bool Z44VerticalQuatrafoil;
    static bool Z42Vertical2ndAstig;
    static bool Z40SphericalAberration;
    static bool Z4neg2Oblique2ndAstig;
    static bool Z4neg4ObliqueQuatrafoil;
    static bool Z33ObliqueTrefoil;
    static bool Z3neg3VerticalTrefoil;
    static bool Z31HorizontalComa;
    static bool Z3neg1VerticalComa;
    static bool Z22VerticalAstig;
    static bool Z2neg2ObliqueAstig;
    static bool Z20Defocus;
    static bool Z11Xtilt;
    static bool Z1neg1Ytilt;
    static bool Z00Piston;

    static bool m_rgb2;
    static bool m_rgb5;
    static bool m_hsbrgb;
    static bool m_gplotpalette;
    static bool m_USSfixed;
    static bool m_perceptualuniformfixed;
    static bool m_USSpalette;
    static bool m_perceptualuniformpalette;

};


#endif // GLWIDGET_H
