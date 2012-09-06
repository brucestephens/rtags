#ifndef Project_h
#define Project_h

#include <tr1/memory>
#include "Path.h"
#include "RTags.h"
#include "ReadWriteLock.h"

class Database;
class Indexer;
class GRTags;
class ScopedDB;
template <typename T>
class Scope
{
public:
    // T *operator->() { return mData->t; }
    T t() { return mData->t; }
private:
    friend class Project;
    struct Data {
        Data(T tt, ReadWriteLock *l)
            : t(tt), lock(l)
        {
        }
        ~Data()
        {
            lock->unlock();
        }
        T t;
        ReadWriteLock *lock;
    };
    shared_ptr<Data> mData;
};
class Project
{
public:
    Project();
    ~Project();

    Indexer *indexer;
    GRTags *grtags;

    Path srcRoot;

    Scope<const SymbolMap&> lockSymbolsForRead();
    Scope<SymbolMap&> lockSymbolsForWrite();
    Scope<const SymbolNameMap&> lockSymbolNamesForRead();
    Scope<SymbolNameMap&> lockSymbolNamesForWrite();

    Scope<const SymbolNameMap&> lockGRForRead();
    Scope<SymbolNameMap&> lockGRForWrite();

    Scope<const Map<Path, Map<ByteArray, time_t> >&> lockGRFilesForRead();
    Scope<Map<Path, Map<ByteArray, time_t> >&> lockGRFilesForWrite();

    void dirty(const Set<uint32_t> &fileIds);
private:
    SymbolMap mSymbols;
    ReadWriteLock mSymbolsLock;

    SymbolNameMap mSymbolNames;
    ReadWriteLock mSymbolNamesLock;

    GRFilesMap mGRFiles;
    ReadWriteLock mGRFilesLock;

    SymbolNameMap mGR;
    ReadWriteLock mGRLock;
};

#endif