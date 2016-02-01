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
import Model.V1.Node
import Model.V1.PersistentVolumeClaimList
import Model.V1.ObjectFieldSelector
import Model.V1.SELinuxOptions
import Model.V1.ContainerStateRunning
import Model.V1.VolumeMount
import Model.V1.PersistentVolumeClaimSpec
import Model.V1.CephFSVolumeSource
import Model.V1.DownwardAPIVolumeSource
import Model.Unversioned.StatusCause
import Model.V1.GCEPersistentDiskVolumeSource
import Model.V1.ResourceQuotaSpec
import Model.V1.NamespaceStatus
import Model.V1.NamespaceSpec
import Model.V1.PersistentVolume
import Model.V1.ConfigMapList
import Model.V1.PersistentVolumeStatus
import Model.V1.EndpointsList
import Model.V1.GitRepoVolumeSource
import Model.V1.ConfigMap
import Model.V1.Capabilities
import Model.V1.PodTemplateList
import Model.V1.NodeCondition
import Model.V1.LocalObjectReference
import Model.V1.ResourceQuotaStatus
import Model.V1.ExecAction
import Model.V1.ObjectMeta
import Model.V1.LimitRangeSpec
import Model.V1.ISCSIVolumeSource
import Model.V1.EmptyDirVolumeSource
import Model.V1.NodeList
import Model.Unversioned.Patch
import Model.V1.PersistentVolumeClaim
import Model.V1.NamespaceList
import Model.V1.ServiceAccount
import Model.V1.NodeAddress
import Model.V1.Namespace
import Model.V1.FlockerVolumeSource
import Model.V1.PersistentVolumeClaimVolumeSource
import Model.Unversioned.ListMeta
import Model.V1.ResourceQuotaList
import Model.V1.PersistentVolumeClaimStatus
import Model.V1.EndpointSubset
import Model.V1.SecretVolumeSource
import Model.V1.FlexVolumeSource
import Model.V1.EnvVarSource
import Model.V1.LoadBalancerIngress
import Model.V1.Service
import Model.V1.ServiceAccountList
import Model.V1.LimitRangeList
import Model.V1.Endpoints
import Model.V1.DeleteOptions
import Model.V1.Volume
-- import Model.integer
import Model.V1.Probe
import Model.V1.SecretKeySelector
import Model.V1.ReplicationController
import Model.V1.Capability
import Model.V1.LimitRange
import Model.V1.DownwardAPIVolumeFile
import Model.V1.PodStatus
import Model.V1.PodSpec
import Model.V1.ContainerPort
import Model.V1.ResourceQuota
import Model.V1.EventList
import Model.V1.Lifecycle
import Model.V1.ReplicationControllerSpec
import Model.V1.NodeStatus
import Model.V1.GlusterfsVolumeSource
import Model.V1.Handler
import Model.V1.EventSource
import Model.V1.PodCondition
import Model.V1.RBDVolumeSource
import Model.V1.PodTemplate
import Model.V1.ServiceStatus
import Model.V1.NFSVolumeSource
import Model.V1.FCVolumeSource
import Model.V1.EndpointPort
import Model.V1.TCPSocketAction
import Model.Unversioned.StatusDetails
import Model.V1.HTTPGetAction
import Model.V1.LoadBalancerStatus
import Model.V1.SecretList
import Model.V1.Container
import Model.V1.PersistentVolumeSpec
import Model.V1.PodSecurityContext
import Model.V1.ReplicationControllerStatus
import Model.V1.FinalizerName
import Model.V1.ServicePort
import Model.V1.ComponentCondition
import Model.V1.ComponentStatusList
import Model.V1.HostPathVolumeSource
import Model.Json.WatchEvent
import Model.V1.ContainerStateTerminated
import Model.V1.Binding
import Model.V1.CinderVolumeSource
import Model.V1.ContainerState
import Model.V1.SecurityContext
import Model.V1.AWSElasticBlockStoreVolumeSource
import Model.V1.ContainerStatus
import Model.V1.ContainerImage
import Model.V1.ReplicationControllerList
import Model.V1.NodeDaemonEndpoints
import Model.V1.Secret
import Model.V1.Event
import Model.V1.EnvVar
import Model.V1.ResourceRequirements
import Model.V1.PersistentVolumeAccessMode
import Model.V1.ComponentStatus
import Model.V1.LimitRangeItem
import Model.V1.PodTemplateSpec
import Model.V1.PodList
import Model.V1.ServiceList
import Model.V1.PersistentVolumeList
import Model.V1.ObjectReference
import Model.V1.ContainerStateWaiting
import Model.Unversioned.Status
import Model.V1.ConfigMapKeySelector
import Model.V1.NodeSystemInfo
import Model.V1.ServiceSpec
import Model.V1.Pod
import Model.V1.NodeSpec
import Model.V1.EndpointAddress
import Model.V1.DaemonEndpoint
import Api.ApivApi

-- userClient :: IO ()
-- userClient = do 
--     users <- sample' (arbitrary :: Gen String)
--     let user = last users
--     void . runEitherT $ do
--         getUserByName user >>= (liftIO . putStrLn . show)

main :: IO ()
main = putStrLn "Hello Server!"
