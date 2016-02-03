{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Servant.API
import Servant.Client

import Data.List.Split (splitOn)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Control.Monad
import Kubernetes.Model.V1.Node
import Kubernetes.Model.V1.PersistentVolumeClaimList
import Kubernetes.Model.V1.ObjectFieldSelector
import Kubernetes.Model.V1.SELinuxOptions
import Kubernetes.Model.V1.ContainerStateRunning
import Kubernetes.Model.V1.VolumeMount
import Kubernetes.Model.V1.PersistentVolumeClaimSpec
import Kubernetes.Model.V1.CephFSVolumeSource
import Kubernetes.Model.V1.DownwardAPIVolumeSource
import Kubernetes.Model.Unversioned.StatusCause
import Kubernetes.Model.V1.GCEPersistentDiskVolumeSource
import Kubernetes.Model.V1.ResourceQuotaSpec
import Kubernetes.Model.V1.NamespaceStatus
import Kubernetes.Model.V1.NamespaceSpec
import Kubernetes.Model.V1.PersistentVolume
import Kubernetes.Model.V1.ConfigMapList
import Kubernetes.Model.V1.PersistentVolumeStatus
import Kubernetes.Model.V1.EndpointsList
import Kubernetes.Model.V1.GitRepoVolumeSource
import Kubernetes.Model.V1.ConfigMap
import Kubernetes.Model.V1.Capabilities
import Kubernetes.Model.V1.PodTemplateList
import Kubernetes.Model.V1.NodeCondition
import Kubernetes.Model.V1.LocalObjectReference
import Kubernetes.Model.V1.ResourceQuotaStatus
import Kubernetes.Model.V1.ExecAction
import Kubernetes.Model.V1.ObjectMeta
import Kubernetes.Model.V1.LimitRangeSpec
import Kubernetes.Model.V1.ISCSIVolumeSource
import Kubernetes.Model.V1.EmptyDirVolumeSource
import Kubernetes.Model.V1.NodeList
import Kubernetes.Model.Unversioned.Patch
import Kubernetes.Model.V1.PersistentVolumeClaim
import Kubernetes.Model.V1.NamespaceList
import Kubernetes.Model.V1.ServiceAccount
import Kubernetes.Model.V1.NodeAddress
import Kubernetes.Model.V1.Namespace
import Kubernetes.Model.V1.FlockerVolumeSource
import Kubernetes.Model.V1.PersistentVolumeClaimVolumeSource
import Kubernetes.Model.Unversioned.ListMeta
import Kubernetes.Model.V1.ResourceQuotaList
import Kubernetes.Model.V1.PersistentVolumeClaimStatus
import Kubernetes.Model.V1.EndpointSubset
import Kubernetes.Model.V1.SecretVolumeSource
import Kubernetes.Model.V1.FlexVolumeSource
import Kubernetes.Model.V1.EnvVarSource
import Kubernetes.Model.V1.LoadBalancerIngress
import Kubernetes.Model.V1.Service
import Kubernetes.Model.V1.ServiceAccountList
import Kubernetes.Model.V1.LimitRangeList
import Kubernetes.Model.V1.Endpoints
import Kubernetes.Model.V1.DeleteOptions
import Kubernetes.Model.V1.Volume
-- import Kubernetes.Model.integer
import Kubernetes.Model.V1.Probe
import Kubernetes.Model.V1.SecretKeySelector
import Kubernetes.Model.V1.ReplicationController
import Kubernetes.Model.V1.Capability
import Kubernetes.Model.V1.LimitRange
import Kubernetes.Model.V1.DownwardAPIVolumeFile
import Kubernetes.Model.V1.PodStatus
import Kubernetes.Model.V1.PodSpec
import Kubernetes.Model.V1.ContainerPort
import Kubernetes.Model.V1.ResourceQuota
import Kubernetes.Model.V1.EventList
import Kubernetes.Model.V1.Lifecycle
import Kubernetes.Model.V1.ReplicationControllerSpec
import Kubernetes.Model.V1.NodeStatus
import Kubernetes.Model.V1.GlusterfsVolumeSource
import Kubernetes.Model.V1.Handler
import Kubernetes.Model.V1.EventSource
import Kubernetes.Model.V1.PodCondition
import Kubernetes.Model.V1.RBDVolumeSource
import Kubernetes.Model.V1.PodTemplate
import Kubernetes.Model.V1.ServiceStatus
import Kubernetes.Model.V1.NFSVolumeSource
import Kubernetes.Model.V1.FCVolumeSource
import Kubernetes.Model.V1.EndpointPort
import Kubernetes.Model.V1.TCPSocketAction
import Kubernetes.Model.Unversioned.StatusDetails
import Kubernetes.Model.V1.HTTPGetAction
import Kubernetes.Model.V1.LoadBalancerStatus
import Kubernetes.Model.V1.SecretList
import Kubernetes.Model.V1.Container
import Kubernetes.Model.V1.PersistentVolumeSpec
import Kubernetes.Model.V1.PodSecurityContext
import Kubernetes.Model.V1.ReplicationControllerStatus
import Kubernetes.Model.V1.FinalizerName
import Kubernetes.Model.V1.ServicePort
import Kubernetes.Model.V1.ComponentCondition
import Kubernetes.Model.V1.ComponentStatusList
import Kubernetes.Model.V1.HostPathVolumeSource
import Kubernetes.Model.Json.WatchEvent
import Kubernetes.Model.V1.ContainerStateTerminated
import Kubernetes.Model.V1.Binding
import Kubernetes.Model.V1.CinderVolumeSource
import Kubernetes.Model.V1.ContainerState
import Kubernetes.Model.V1.SecurityContext
import Kubernetes.Model.V1.AWSElasticBlockStoreVolumeSource
import Kubernetes.Model.V1.ContainerStatus
import Kubernetes.Model.V1.ContainerImage
import Kubernetes.Model.V1.ReplicationControllerList
import Kubernetes.Model.V1.NodeDaemonEndpoints
import Kubernetes.Model.V1.Secret
import Kubernetes.Model.V1.Event
import Kubernetes.Model.V1.EnvVar
import Kubernetes.Model.V1.ResourceRequirements
import Kubernetes.Model.V1.PersistentVolumeAccessMode
import Kubernetes.Model.V1.ComponentStatus
import Kubernetes.Model.V1.LimitRangeItem
import Kubernetes.Model.V1.PodTemplateSpec
import Kubernetes.Model.V1.PodList
import Kubernetes.Model.V1.ServiceList
import Kubernetes.Model.V1.PersistentVolumeList
import Kubernetes.Model.V1.ObjectReference
import Kubernetes.Model.V1.ContainerStateWaiting
import Kubernetes.Model.Unversioned.Status
import Kubernetes.Model.V1.ConfigMapKeySelector
import Kubernetes.Model.V1.NodeSystemInfo
import Kubernetes.Model.V1.ServiceSpec
import Kubernetes.Model.V1.Pod
import Kubernetes.Model.V1.NodeSpec
import Kubernetes.Model.V1.EndpointAddress
import Kubernetes.Model.V1.DaemonEndpoint
import Kubernetes.Api.ApivApi

-- userClient :: IO ()
-- userClient = do 
--     users <- sample' (arbitrary :: Gen String)
--     let user = last users
--     void . runEitherT $ do
--         getUserByName user >>= (liftIO . putStrLn . show)

main :: IO ()
main = putStrLn "Hello Server!"
