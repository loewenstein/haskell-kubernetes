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
import Model.v1.Node
import Model.v1.PersistentVolumeClaimList
import Model.v1.ObjectFieldSelector
import Model.v1.SELinuxOptions
import Model.v1.ContainerStateRunning
import Model.v1.VolumeMount
import Model.v1.PersistentVolumeClaimSpec
import Model.v1.CephFSVolumeSource
import Model.v1.DownwardAPIVolumeSource
import Model.unversioned.StatusCause
import Model.v1.GCEPersistentDiskVolumeSource
import Model.v1.ResourceQuotaSpec
import Model.v1.NamespaceStatus
import Model.v1.NamespaceSpec
import Model.v1.PersistentVolume
import Model.v1.ConfigMapList
import Model.v1.PersistentVolumeStatus
import Model.v1.EndpointsList
import Model.v1.GitRepoVolumeSource
import Model.v1.ConfigMap
import Model.v1.Capabilities
import Model.v1.PodTemplateList
import Model.v1.NodeCondition
import Model.v1.LocalObjectReference
import Model.v1.ResourceQuotaStatus
import Model.v1.ExecAction
import Model.v1.ObjectMeta
import Model.v1.LimitRangeSpec
import Model.v1.ISCSIVolumeSource
import Model.v1.EmptyDirVolumeSource
import Model.v1.NodeList
import Model.unversioned.Patch
import Model.v1.PersistentVolumeClaim
import Model.v1.NamespaceList
import Model.v1.ServiceAccount
import Model.v1.NodeAddress
import Model.v1.Namespace
import Model.v1.FlockerVolumeSource
import Model.v1.PersistentVolumeClaimVolumeSource
import Model.unversioned.ListMeta
import Model.v1.ResourceQuotaList
import Model.v1.PersistentVolumeClaimStatus
import Model.v1.EndpointSubset
import Model.v1.SecretVolumeSource
import Model.v1.FlexVolumeSource
import Model.v1.EnvVarSource
import Model.v1.LoadBalancerIngress
import Model.v1.Service
import Model.v1.ServiceAccountList
import Model.v1.LimitRangeList
import Model.v1.Endpoints
import Model.v1.DeleteOptions
import Model.v1.Volume
import Model.integer
import Model.v1.Probe
import Model.v1.SecretKeySelector
import Model.v1.ReplicationController
import Model.v1.Capability
import Model.v1.LimitRange
import Model.v1.DownwardAPIVolumeFile
import Model.v1.PodStatus
import Model.v1.PodSpec
import Model.v1.ContainerPort
import Model.v1.ResourceQuota
import Model.v1.EventList
import Model.v1.Lifecycle
import Model.v1.ReplicationControllerSpec
import Model.v1.NodeStatus
import Model.v1.GlusterfsVolumeSource
import Model.v1.Handler
import Model.v1.EventSource
import Model.v1.PodCondition
import Model.v1.RBDVolumeSource
import Model.v1.PodTemplate
import Model.v1.ServiceStatus
import Model.v1.NFSVolumeSource
import Model.v1.FCVolumeSource
import Model.v1.EndpointPort
import Model.v1.TCPSocketAction
import Model.unversioned.StatusDetails
import Model.v1.HTTPGetAction
import Model.v1.LoadBalancerStatus
import Model.v1.SecretList
import Model.v1.Container
import Model.v1.PersistentVolumeSpec
import Model.v1.PodSecurityContext
import Model.v1.ReplicationControllerStatus
import Model.v1.FinalizerName
import Model.v1.ServicePort
import Model.v1.ComponentCondition
import Model.v1.ComponentStatusList
import Model.v1.HostPathVolumeSource
import Model.json.WatchEvent
import Model.v1.ContainerStateTerminated
import Model.v1.Binding
import Model.v1.CinderVolumeSource
import Model.v1.ContainerState
import Model.v1.SecurityContext
import Model.v1.AWSElasticBlockStoreVolumeSource
import Model.v1.ContainerStatus
import Model.v1.ContainerImage
import Model.v1.ReplicationControllerList
import Model.v1.NodeDaemonEndpoints
import Model.v1.Secret
import Model.v1.Event
import Model.v1.EnvVar
import Model.v1.ResourceRequirements
import Model.v1.PersistentVolumeAccessMode
import Model.v1.ComponentStatus
import Model.v1.LimitRangeItem
import Model.v1.PodTemplateSpec
import Model.v1.PodList
import Model.v1.ServiceList
import Model.v1.PersistentVolumeList
import Model.v1.ObjectReference
import Model.v1.ContainerStateWaiting
import Model.unversioned.Status
import Model.v1.ConfigMapKeySelector
import Model.v1.NodeSystemInfo
import Model.v1.ServiceSpec
import Model.v1.Pod
import Model.v1.NodeSpec
import Model.v1.EndpointAddress
import Model.v1.DaemonEndpoint
import Api.ApivApi

-- userClient :: IO ()
-- userClient = do 
--     users <- sample' (arbitrary :: Gen String)
--     let user = last users
--     void . runEitherT $ do
--         getUserByName user >>= (liftIO . putStrLn . show)

main :: IO ()
main = putStrLn "Hello Server!"
