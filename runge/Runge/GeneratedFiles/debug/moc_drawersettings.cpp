/****************************************************************************
** Meta object code from reading C++ file 'drawersettings.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.2.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../drawersettings.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'drawersettings.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.2.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
struct qt_meta_stringdata_DrawerSettings_t {
    QByteArrayData data[8];
    char stringdata[90];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    offsetof(qt_meta_stringdata_DrawerSettings_t, stringdata) + ofs \
        - idx * sizeof(QByteArrayData) \
    )
static const qt_meta_stringdata_DrawerSettings_t qt_meta_stringdata_DrawerSettings = {
    {
QT_MOC_LITERAL(0, 0, 14),
QT_MOC_LITERAL(1, 15, 11),
QT_MOC_LITERAL(2, 27, 0),
QT_MOC_LITERAL(3, 28, 16),
QT_MOC_LITERAL(4, 45, 6),
QT_MOC_LITERAL(5, 52, 11),
QT_MOC_LITERAL(6, 64, 15),
QT_MOC_LITERAL(7, 80, 8)
    },
    "DrawerSettings\0onButtonBox\0\0"
    "QAbstractButton*\0button\0onAxesColor\0"
    "onAdjustLengths\0showHelp\0"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_DrawerSettings[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: name, argc, parameters, tag, flags
       1,    1,   34,    2, 0x09,
       5,    0,   37,    2, 0x09,
       6,    0,   38,    2, 0x09,
       7,    0,   39,    2, 0x09,

 // slots: parameters
    QMetaType::Void, 0x80000000 | 3,    4,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,

       0        // eod
};

void DrawerSettings::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        DrawerSettings *_t = static_cast<DrawerSettings *>(_o);
        switch (_id) {
        case 0: _t->onButtonBox((*reinterpret_cast< QAbstractButton*(*)>(_a[1]))); break;
        case 1: _t->onAxesColor(); break;
        case 2: _t->onAdjustLengths(); break;
        case 3: _t->showHelp(); break;
        default: ;
        }
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        switch (_id) {
        default: *reinterpret_cast<int*>(_a[0]) = -1; break;
        case 0:
            switch (*reinterpret_cast<int*>(_a[1])) {
            default: *reinterpret_cast<int*>(_a[0]) = -1; break;
            case 0:
                *reinterpret_cast<int*>(_a[0]) = qRegisterMetaType< QAbstractButton* >(); break;
            }
            break;
        }
    }
}

const QMetaObject DrawerSettings::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_DrawerSettings.data,
      qt_meta_data_DrawerSettings,  qt_static_metacall, 0, 0}
};


const QMetaObject *DrawerSettings::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *DrawerSettings::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_DrawerSettings.stringdata))
        return static_cast<void*>(const_cast< DrawerSettings*>(this));
    if (!strcmp(_clname, "Ui::DrawerSettings"))
        return static_cast< Ui::DrawerSettings*>(const_cast< DrawerSettings*>(this));
    return QDialog::qt_metacast(_clname);
}

int DrawerSettings::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 4)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 4;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 4)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 4;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
