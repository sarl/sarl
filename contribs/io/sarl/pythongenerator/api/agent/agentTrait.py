from contribs.io.sarl.pythongenerator.api.agent import Agent


class AgentTrait:

    def __init__(self, agent: Agent = None):
        self.__agentRef = agent

    def setOwner(self, agent):
        self.__agentRef = agent

    def getOwner(self):
        return self.__agentRef

