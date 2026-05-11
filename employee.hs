type EId = String

type Date = (Int, Int, Int)

data WorkPermit = Permit { number :: String, expiryDate :: Date }
  deriving (Show, Eq)

data Employee = Emp
  {
    empId    :: EId,
    joinedOn :: Date,
    permit   :: Maybe WorkPermit,
    leftOn   :: Maybe Date
  }
  deriving (Show, Eq)