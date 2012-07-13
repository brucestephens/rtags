#include "DirtyThread.h"
#include "Database.h"
#include "RTags.h"
#include "Server.h"

DirtyThread::DirtyThread(const Set<uint32_t> &dirtyFileIds, ScopedDB symbols, ScopedDB symbolNames)
    : mDirtyFileIds(dirtyFileIds), mSymbols(symbols), mSymbolNames(symbolNames)
{
    setAutoDelete(true);
    assert(mSymbols.lockType() == ReadWriteLock::Write);
    assert(mSymbolNames.lockType() == ReadWriteLock::Write);
}

static inline void dirtySymbolNames(ScopedDB &db, const Set<uint32_t> &dirty)
{
    RTags::Ptr<Iterator> it(db->createIterator());
    it->seekToFirst();
    while (it->isValid()) {
        Set<Location> locations = it->value<Set<Location> >();
        Set<Location>::iterator i = locations.begin();
        bool changed = false;
        while (i != locations.end()) {
            if (dirty.contains(i->fileId())) {
                changed = true;
                locations.erase(i++);
            } else {
                ++i;
            }
        }
        if (changed) {
            if (locations.isEmpty()) {
                debug() << "No references to " << it->key() << " anymore. Removing";
                db->remove(it->key());
            } else {
                debug() << "References to " << it->key() << " modified. Changing";
                db->setValue<Set<Location> >(it->key(), locations);
            }
        }
        it->next();
    }
}

static inline int dirtySymbols(ScopedDB &db, const Set<uint32_t> &dirty)
{
    int ret = 0;
    RTags::Ptr<Iterator> it(db->createIterator());
    Batch batch(db);
    it->seekToFirst();
    while (it->isValid()) {
        const Slice key = it->key();
        assert(key.size() == 8);
        const Location loc = Location::fromKey(key.data());
        if (dirty.contains(loc.fileId())) {
            batch.remove(key);
            ++ret;
        } else {
            CursorInfo cursorInfo = it->value<CursorInfo>();
            if (cursorInfo.dirty(dirty)) {
                batch.add(key, cursorInfo);
                ++ret;
            }
        }
        it->next();
    }

    return ret;
}

void DirtyThread::run()
{
    dirtySymbols(mSymbols, mDirtyFileIds);
    dirtySymbolNames(mSymbolNames, mDirtyFileIds);
}