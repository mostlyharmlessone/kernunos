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

using namespace Qt::StringLiterals;

Assistant::~Assistant()
{
    if (!m_process.isNull() && m_process->state() == QProcess::Running) {
        QObject::disconnect(m_process.data(), &QProcess::finished, nullptr, nullptr);
        m_process->terminate();
        m_process->waitForFinished(3000);
    }
}

void Assistant::showDocumentation(const QString &page)
{
    if (!startAssistant())
        return;

    QByteArray ba("SetSource ");
//    ba.append("qthelp://org.qt-project.examples.simpletextviewer/doc/");   //this corresponds to qhp/qhcp files
    ba.append("qthelp://Janus/kernunos/doc/");   //this corresponds to qhp/qhcp files
    m_process->write(ba + page.toLocal8Bit() + '\n');

    std::cout << "Qt Assistant: " << (ba + page.toLocal8Bit() + '\n').toStdString() << "\n";

    if (!startAssistant())
        return;
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
        const QString path = dir + "/documentation";

        std::cout << "Qt Assistant path to documentation: " << path.toStdString() << "\n";

        if (QFileInfo::exists(path))
            return path;
    }
    return {};
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
        QString app = QLibraryInfo::path(QLibraryInfo::BinariesPath);
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
                               collectionDirectory + "/simpletextviewer.qhc",
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
