{-# LANGUAGE OverloadedStrings #-}

module ExampleService () where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict                           as HashMap
import           Data.Text                                     hiding (head,
                                                                map)
import qualified Kubernetes.Model.V1.Any                       as Any
import qualified Kubernetes.Model.V1.ObjectMeta                as OM
import qualified Kubernetes.Model.V1.Service     as SVC
import qualified Kubernetes.Model.V1.ServiceSpec as SVCSpec
import qualified Kubernetes.Model.V1.ServicePort as SVCPort
import qualified Kubernetes.Utils as KUtils


exampleSVC :: SVC.Service
exampleSVC = SVC.Service {
  SVC._kind = Just "Service",
  SVC._apiVersion = Just "v1",
  SVC._metadata = Just OM.ObjectMeta {
    OM._name = Just "testapp-service",
    OM._generateName = Nothing,
    OM._namespace = Nothing,
    OM._selfLink = Nothing,
    OM._uid = Nothing,
    OM._resourceVersion = Nothing,
    OM._generation = Nothing,
    OM._creationTimestamp = Nothing,
    OM._deletionTimestamp = Nothing,
    OM._deletionGracePeriodSeconds = Nothing,
    OM._labels = Nothing,
    OM._annotations = Nothing
  },
  SVC._spec = Just SVCSpec.ServiceSpec {
    SVCSpec._ports = [
      SVCPort.ServicePort {
        SVCPort._name = Nothing,
        SVCPort._protocol = Just "TCP",
        SVCPort._port = 80,
        SVCPort._targetPort = Just KUtils.IntegerOrText {
          KUtils.unIntOrText = Left 80
        },
        SVCPort._nodePort = Just 30060
      }
    ],
    SVCSpec._selector = Just Any.Any {
      Any._any = HashMap.fromList [("app", "testapp")]
    },
    SVCSpec._clusterIP = Nothing,
    SVCSpec._type_ = Just "NodePort",
    SVCSpec._externalIPs = Nothing,
    SVCSpec._deprecatedPublicIPs = Nothing,
    SVCSpec._sessionAffinity = Nothing,
    SVCSpec._loadBalancerIP = Nothing
  },
  SVC._status = Nothing
}

main = print $ encode exampleSVC
