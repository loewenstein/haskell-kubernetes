{-# LANGUAGE OverloadedStrings #-}

module ExampleReplicationController () where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict                           as HashMap
import           Data.Text                                     hiding (head,
                                                                map)
import qualified Kubernetes.Model.V1.Any                       as Any
import qualified Kubernetes.Model.V1.Container                 as CO
import qualified Kubernetes.Model.V1.ContainerPort             as COPort
import qualified Kubernetes.Model.V1.ObjectMeta                as OM
import qualified Kubernetes.Model.V1.PodSpec                   as POSpec
import qualified Kubernetes.Model.V1.PodTemplateSpec           as PodTemplSpec
import qualified Kubernetes.Model.V1.ReplicationController     as RC
import qualified Kubernetes.Model.V1.ReplicationControllerSpec as RCSpec


exampleRC :: RC.ReplicationController
exampleRC = RC.ReplicationController {
  RC._kind = Just "ReplicationController",
  RC._apiVersion = Just "v1",
  RC._metadata = Just OM.ObjectMeta {
    OM._name = Just "test-app",
    OM._generateName = Nothing,
    OM._namespace = Nothing,
    OM._selfLink = Nothing,
    OM._uid = Nothing,
    OM._resourceVersion = Nothing,
    OM._generation = Nothing,
    OM._creationTimestamp = Nothing,
    OM._deletionTimestamp = Nothing,
    OM._deletionGracePeriodSeconds = Nothing,
    OM._labels = Just Any.Any {
      Any._any = HashMap.fromList [("customer", "testcustomer"), ("service", "testservice")]
    },
    OM._annotations = Nothing
  },
  RC._spec = Just RCSpec.ReplicationControllerSpec {
    RCSpec._replicas = Just 1,
    RCSpec._selector = Just Any.Any {
      Any._any = HashMap.fromList [("app", "testapp")]
    },
    RCSpec._template = Just examplePodTemplateSpec
  },
  RC._status = Nothing
}

examplePodTemplateSpec = PodTemplSpec.PodTemplateSpec {
  PodTemplSpec._metadata = Just OM.ObjectMeta {
    OM._name = Nothing,
    OM._generateName = Nothing,
    OM._namespace = Nothing,
    OM._selfLink = Nothing,
    OM._uid = Nothing,
    OM._resourceVersion = Nothing,
    OM._generation = Nothing,
    OM._creationTimestamp = Nothing,
    OM._deletionTimestamp = Nothing,
    OM._deletionGracePeriodSeconds = Nothing,
    OM._labels = Just Any.Any {
      Any._any = HashMap.fromList [("app", "testapp")]
    },
    OM._annotations = Nothing
  },
  PodTemplSpec._spec = Just POSpec.PodSpec {
    POSpec._volumes = Nothing,
    POSpec._containers = [CO.Container {
      CO._name = "nginx",
      CO._image = Just "nginx",
      CO._command = Nothing,
      CO._args = Nothing,
      CO._workingDir = Nothing,
      CO._ports = Just [COPort.ContainerPort {
        COPort._name = Nothing,
        COPort._hostPort = Nothing,
        COPort._containerPort = 80,
        COPort._protocol = Just "TCP",
        COPort._hostIP = Nothing
      }],
      CO._env = Nothing,
      CO._resources = Nothing,
      CO._volumeMounts = Nothing,
      CO._livenessProbe = Nothing,
      CO._readinessProbe = Nothing,
      CO._lifecycle = Nothing,
      CO._terminationMessagePath = Nothing,
      CO._imagePullPolicy = Nothing,
      CO._securityContext = Nothing,
      CO._stdin = Nothing,
      CO._stdinOnce = Nothing,
      CO._tty = Nothing
    }],
    POSpec._restartPolicy = Just "Always",
    POSpec._terminationGracePeriodSeconds = Nothing,
    POSpec._activeDeadlineSeconds = Nothing,
    POSpec._dnsPolicy = Just "ClusterFirst",
    POSpec._nodeSelector = Nothing,
    POSpec._serviceAccountName = Nothing,
    POSpec._serviceAccount = Nothing,
    POSpec._nodeName = Nothing,
    POSpec._hostNetwork = Nothing,
    POSpec._hostPID = Nothing,
    POSpec._hostIPC = Nothing,
    POSpec._securityContext = Nothing,
    POSpec._imagePullSecrets = Nothing
  }

}

main :: IO ()
main = print $ encode exampleRC
