#ifndef GRTags_h
#define GRTags_h

#include "Path.h"
#include "List.h"
#include "FileSystemWatcher.h"
#include "Mutex.h"
#include "Location.h"

class ScopedDB;
class GRParseJob;
class Project;
class GRTags
{
public:
    GRTags();
    enum Flag {
        None = 0x0,
        Parse = 0x1
    };
    void init(const shared_ptr<Project> &proj, unsigned flags);
    void enableParsing();
    bool parsingEnabled() const { return mFlags & Parse; }
    unsigned flags() const { return mFlags; }
    void recurseDirs();
    void onFileAdded(const Path &path);
    void onFileRemoved(const Path &path);
    void onFileModified(const Path &path);
    void onRecurseJobFinished(Map<Path, bool> &mPaths);
    void onParseJobFinished(GRParseJob *job, const Map<ByteArray, Map<Location, bool> > &entries);
    void dirty(uint32_t fileId, ScopedDB &db);
    void parse(const Path &path, unsigned flags);
    void addFile(const Path &file, time_t time, ScopedDB *db);
    void removeFile(const Path &file, ScopedDB *grfiles = 0);
private:
    FileSystemWatcher *mWatcher;
    weak_ptr<Project> mProject;
    Path mSrcRoot;
    friend class FindFileJob;
    mutable Mutex mMutex;
    int mCount, mActive;
    unsigned mFlags;
    const List<ByteArray> &mFilters;
};

#endif