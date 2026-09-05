// Copyright (C) 2017 The Qt Company Ltd.
// SPDX-License-Identifier: LicenseRef-Qt-Commercial OR BSD-3-Clause
// minimal changes from simpletextviewer example

#include "assistant.h"

#include <QApplication>
#include <QByteArray>
#include <QDir>
#include <QLibraryInfo>
#include <QMessageBox>
#include <QStandardPaths>
#include <iostream>

extern "C" {
void LogC(const char *Message);
}

using namespace Qt::StringLiterals;

Assistant::~Assistant()
{
    if (!m_process.isNull() && m_process->state() == QProcess::Running) {
        QObject::disconnect(m_process.data(), &QProcess::finished, nullptr, nullptr);
        m_process->terminate();
        m_process->waitForFinished(3000);
    }
}

static QString documentationDirectory()
{
    QStringList paths;
#ifdef SRCDIR
    paths.append(QLatin1StringView(SRCDIR));
#endif
    paths.append(QCoreApplication::applicationDirPath());
    paths.append(QStandardPaths::standardLocations(QStandardPaths::AppDataLocation));
    for (const auto &dir : std::as_const(paths)) {
        const QString path = dir + "/documentation/";
#if defined(__APPLE__)
        std::string str0 = path.toUtf8().constData();
        LogC(("Qt Assistant path: " + str0).c_str());
#else
        LogC(("Qt Assistant path: " + path.toStdString()).c_str());
#endif
        if (QFileInfo::exists(path))
            return path;
    }
    return {};
}

void Assistant::showDocumentation(const QString &page)
{
    if (!startAssistant())
        return;
    const QString collectionDirectory = documentationDirectory();
    QByteArray ba("SetSource ");
#if defined(__APPLE__)
    std::string str = collectionDirectory.toUtf8().constData();
    ba.append("qthelp:" + str);
#else
    ba.append("qthelp:" + collectionDirectory.toStdString());
#endif
    m_process->write(ba + page.toLocal8Bit() + '\n');
#if defined(__APPLE__)
    std::string str2 = QString::fromUtf8(ba + page.toLocal8Bit() + '\n').toUtf8().constData();
    ba.append("qthelp:" + str2);
    LogC(("Qt Assistant: " + str2).c_str());
#else
    LogC(("Qt Assistant: " + (ba + page.toLocal8Bit() + '\n').toStdString()).c_str());
#endif
    if (!startAssistant())
        return;
}



bool Assistant::startAssistant()
{
    if (m_process.isNull()) {
        m_process.reset(new QProcess);
        QObject::connect(m_process.data(), &QProcess::finished,
                         m_process.data(), [this](int exitCode, QProcess::ExitStatus status) {
            finished(exitCode, status);
        });
    }

    if (m_process->state() != QProcess::Running) {
#ifdef _WIN32
      QString app = ".";
#else
      QString app = QLibraryInfo::path(QLibraryInfo::BinariesPath);
#endif
#ifndef Q_OS_DARWIN
        app += "/assistant"_L1;
#else
        app += "/Assistant.app/Contents/MacOS/Assistant"_L1;
#endif

        const QString collectionDirectory = documentationDirectory();
        if (collectionDirectory.isEmpty()) {
            showError(tr("The documentation directory cannot be found"));
            return false;
        }

        const QStringList args{"-collectionFile",
                               collectionDirectory + "/kernunos.qhc",
                               "-enableRemoteControl"};


        m_process->start(app, args);

        if (!m_process->waitForStarted(3000)) {
            showError(tr("Unable to launch Qt Assistant (%1): %2")
                      .arg(QDir::toNativeSeparators(app), m_process->errorString()));
            return false;
        }
    }
    return true;
}

void Assistant::showError(const QString &message)
{
    QMessageBox::critical(QApplication::activeWindow(), tr("kernunos"), message);
}

void Assistant::finished(int exitCode, QProcess::ExitStatus status)
{
    const QString stdErr = QString::fromLocal8Bit(m_process->readAllStandardError());
    if (status != QProcess::NormalExit)
        showError(tr("Assistant crashed: %1").arg(stdErr));
    else if (exitCode != 0)
        showError(tr("Assistant exited with %1: %2").arg(exitCode).arg(stdErr));
}
