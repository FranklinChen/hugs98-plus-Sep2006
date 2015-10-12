#pragma once

extern "C" {
extern System::Object __gc* getNP(Cell c);
Cell   mkDotNetPtr     (System::Object *, Void (*)(System::Object *));
Void incDotNetPtrRefCnt(Int, Int);
Void freeNetPtr (System::Object* x);
};
