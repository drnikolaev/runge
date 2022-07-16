/****************************************************************************
** Meta object code from reading C++ file 'drawer.h'
**
** Created by: The Qt Meta Object Compiler version 67 (Qt 5.2.0)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../drawer.h"
#include <QtCore/qbytearray.h>
#include <QtCore/qmetatype.h>
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'drawer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 67
#error "This file was generated using the moc from 5.2.0. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
struct qt_meta_stringdata_Drawer_t {
    QByteArrayData data[15];
    char stringdata[157];
};
#define QT_MOC_LITERAL(idx, ofs, len) \
    Q_STATIC_BYTE_ARRAY_DATA_HEADER_INITIALIZER_WITH_OFFSET(len, \
    offsetof(qt_meta_stringdata_Drawer_t, stringdata) + ofs \
        - idx * sizeof(QByteArrayData) \
    )
static const qt_meta_stringdata_Drawer_t qt_meta_stringdata_Drawer = {
    {
QT_MOC_LITERAL(0, 0, 6),
QT_MOC_LITERAL(1, 7, 4),
QT_MOC_LITERAL(2, 12, 0),
QT_MOC_LITERAL(3, 13, 8),
QT_MOC_LITERAL(4, 22, 5),
QT_MOC_LITERAL(5, 28, 15),
QT_MOC_LITERAL(6, 44, 5),
QT_MOC_LITERAL(7, 50, 15),
QT_MOC_LITERAL(8, 66, 9),
QT_MOC_LITERAL(9, 76, 14),
QT_MOC_LITERAL(10, 91, 5),
QT_MOC_LITERAL(11, 97, 16),
QT_MOC_LITERAL(12, 114, 14),
QT_MOC_LITERAL(13, 129, 13),
QT_MOC_LITERAL(14, 143, 12)
    },
    "Drawer\0zoom\0\0QAction*\0color\0xAxisVarChanged\0"
    "index\0yAxisVarChanged\0onRunFrom\0"
    "onButtonDelete\0onPan\0onButtonSettings\0"
    "onButtonExport\0onButtonPrint\0onButtonHelp\0"
};
#undef QT_MOC_LITERAL

static const uint qt_meta_data_Drawer[] = {

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
       7,    1,   78,    2, 0x08,
       8,    0,   81,    2, 0x08,
       9,    0,   82,    2, 0x08,
      10,    0,   83,    2, 0x08,
      11,    0,   84,    2, 0x08,
      12,    0,   85,    2, 0x08,
      13,    0,   86,    2, 0x08,
      14,    0,   87,    2, 0x08,

 // slots: parameters
    QMetaType::Void, 0x80000000 | 3,    2,
    QMetaType::Void, 0x80000000 | 3,    2,
    QMetaType::Void, QMetaType::Int,    6,
    QMetaType::Void, QMetaType::Int,    6,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,
    QMetaType::Void,

       0        // eod
};

void Drawer::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Drawer *_t = static_cast<Drawer *>(_o);
        switch (_id) {
        case 0: _t->zoom((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        case 1: _t->color((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        case 2: _t->xAxisVarChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: _t->yAxisVarChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: _t->onRunFrom(); break;
        case 5: _t->onButtonDelete(); break;
        case 6: _t->onPan(); break;
        case 7: _t->onButtonSettings(); break;
        case 8: _t->onButtonExport(); break;
        case 9: _t->onButtonPrint(); break;
        case 10: _t->onButtonHelp(); break;
        default: ;
        }
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        switch (_id) {
        default: *reinterpret_cast<int*>(_a[0]) = -1; break;
        case 0:
            switch (*reinterpret_cast<int*>(_a[1])) {
            default: *reinterpret_cast<int*>(_a[0]) = -1; break;
            case 0:
                *reinterpret_cast<int*>(_a[0]) = qRegisterMetaType< QAction* >(); break;
            }
            break;
        case 1:
            switch (*reinterpret_cast<int*>(_a[1])) {
            default: *reinterpret_cast<int*>(_a[0]) = -1; break;
            case 0:
                *reinterpret_cast<int*>(_a[0]) = qRegisterMetaType< QAction* >(); break;
            }
            break;
        }
    }
}

const QMetaObject Drawer::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_Drawer.data,
      qt_meta_data_Drawer,  qt_static_metacall, 0, 0}
};


const QMetaObject *Drawer::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &staticMetaObject;
}

void *Drawer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Drawer.stringdata))
        return static_cast<void*>(const_cast< Drawer*>(this));
    if (!strcmp(_clname, "Ui::Drawer"))
        return static_cast< Ui::Drawer*>(const_cast< Drawer*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int Drawer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
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
            qt_static_metacall(this, _c, _id, _a);
        _id -= 11;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
