/*
 * Keeping track of progress towards a goal.
 */
#ifndef __GOAL_H__
#define __GOAL_H__

typedef long   Target;
extern Void setGoal Args((String,Target));
extern Void soFar   Args((Target));
extern Void done    Args((Void));
extern Void failed  Args((Void));

#endif /* __GOAL_H__ */
