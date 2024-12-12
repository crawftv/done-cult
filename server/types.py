from pydantic import BaseModel


class SaveData(BaseModel):
    notKnowingTasks: dict
    actionTasks: dict
    doneTasks: dict
    destroyedTasks: dict
