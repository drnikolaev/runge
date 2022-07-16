/****************************************************************************
** Meta object code from reading C++ file 'drawer3d.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.2.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../drawer3d.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'drawer3d.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.2.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
struct qt_meta_stringdata_Drawer3D_t {
    QByteArrayData data[14];
    char stringdata[182];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    offsetof(qt_meta_stringdata_Drawer3D_t, stringdata) + ofs \
        - idx * sizeof(QByteArrayData) \
    )
static const qt_meta_stringdata_Drawer3D_t qt_meta_stringdata_Drawer3D = {
    {
QT_MOC_LITERAL(0, 0, 8),
QT_MOC_LITERAL(1, 9, 15),
QT_MOC_LITERAL(2, 25, 0),
QT_MOC_LITERAL(3, 26, 5),
QT_MOC_LITERAL(4, 32, 15),
QT_MOC_LITERAL(5, 48, 15),
QT_MOC_LITERAL(6, 64, 15),
QT_MOC_LITERAL(7, 80, 15),
QT_MOC_LITERAL(8, 96, 15),
QT_MOC_LITERAL(9, 112, 17),
QT_MOC_LITERAL(10, 130, 5),
QT_MOC_LITERAL(11, 136, 14),
QT_MOC_LITERAL(12, 151, 16),
QT_MOC_LITERAL(13, 168, 12)
    },
    "Drawer3D\0xAxisVarChanged\0\0index\0"
    "yAxisVarChanged\0zAxisVarChanged\0"
    "xSpinBoxChanged\0ySpinBoxChanged\0"
    "zSpinBoxChanged\0zoomFactorChanged\0"
    "onPan\0onButtonExport\0onButtonSettings\0"
    "onButtonHelp\0"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_Drawer3D[] = {

 // content:
       7,       // revision
       0,       // classname
       0,    0, // classinfo
      11,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: name, argc, parameters, tag, flags
       1,    1,   69,    2, 0x08,
       4,    1,   72,    2, 0x08,
       5,    1,   75,    2, 0x08,
       6,    1,   78,    2, 0x08,
       7,    1,   81,    2, 0x08,
       8,    1,   84,    2, 0x08,
       9,    1,   87,    2, 0x08,
      10,    0,   90,    2, 0x08,
      11,    0,   91,    2, 0x08,
      12,    0,   92,    2, 0x08,
      13,    0,   93,    2, 0x08,

 // slots: parameters
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Int,    3,
    QMetaType::Void, QMetaType::Double,    2,
    QMetaType::Void, QMetaType::Double,    2,
    QMetaType::Void, QMetaType::Double,    2,
    QMetaType::Void, QMetaType::Double,    2,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,

       0        // eod
};

void Drawer3D::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Drawer3D *_t = static_cast<Drawer3D *>(_o);
        switch (_id) {
        case 0: _t->xAxisVarChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: _t->yAxisVarChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: _t->zAxisVarChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: _t->xSpinBoxChanged((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 4: _t->ySpinBoxChanged((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 5: _t->zSpinBoxChanged((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 6: _t->zoomFactorChanged((*reinterpret_cast< double(*)>(_a[1]))); break;
        case 7: _t->onPan(); break;
        case 8: _t->onButtonExport(); break;
        case 9: _t->onButtonSettings(); break;
        case 10: _t->onButtonHelp(); break;
        default: ;
        }
    }
}

const QMetaObject Drawer3D::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_Drawer3D.data,
      qt_meta_data_Drawer3D,  qt_static_metacall, 0, 0}
};


const QMetaObject *Drawer3D::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *Drawer3D::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Drawer3D.stringdata))
        return static_cast<void*>(const_cast< Drawer3D*>(this));
    if (!strcmp(_clname, "Ui::Drawer3D"))
        return static_cast< Ui::Drawer3D*>(const_cast< Drawer3D*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int Drawer3D::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 11)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 11;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < 11)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= 11;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
