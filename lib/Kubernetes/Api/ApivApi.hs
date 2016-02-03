-- This source code is distributed under the terms of a BSD license,
-- Copyright (c) 2016-present, SoundCloud Ltd.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Kubernetes.Api.ApivApi (
      getAPIResources
    , listNamespacedComponentStatus
    , readNamespacedComponentStatus
    , listConfigMap
    , listEndpoints
    , listEvent
    , listLimitRange
    , listNamespacedNamespace
    , createNamespacedNamespace
    , deletecollectionNamespacedNamespace
    , createNamespacedBinding
    , listNamespacedConfigMap
    , createNamespacedConfigMap
    , deletecollectionNamespacedConfigMap
    , readNamespacedConfigMap
    , replaceNamespacedConfigMap
    , deleteNamespacedConfigMap
    , patchNamespacedConfigMap
    , listNamespacedEndpoints
    , createNamespacedEndpoints
    , deletecollectionNamespacedEndpoints
    , readNamespacedEndpoints
    , replaceNamespacedEndpoints
    , deleteNamespacedEndpoints
    , patchNamespacedEndpoints
    , listNamespacedEvent
    , createNamespacedEvent
    , deletecollectionNamespacedEvent
    , readNamespacedEvent
    , replaceNamespacedEvent
    , deleteNamespacedEvent
    , patchNamespacedEvent
    , listNamespacedLimitRange
    , createNamespacedLimitRange
    , deletecollectionNamespacedLimitRange
    , readNamespacedLimitRange
    , replaceNamespacedLimitRange
    , deleteNamespacedLimitRange
    , patchNamespacedLimitRange
    , listNamespacedPersistentVolumeClaim
    , createNamespacedPersistentVolumeClaim
    , deletecollectionNamespacedPersistentVolumeClaim
    , readNamespacedPersistentVolumeClaim
    , replaceNamespacedPersistentVolumeClaim
    , deleteNamespacedPersistentVolumeClaim
    , patchNamespacedPersistentVolumeClaim
    , replaceNamespacedPersistentVolumeClaimStatus
    , listNamespacedPod
    , createNamespacedPod
    , deletecollectionNamespacedPod
    , readNamespacedPod
    , replaceNamespacedPod
    , deleteNamespacedPod
    , patchNamespacedPod
    , connectGetNamespacedPodAttach
    , connectPostNamespacedPodAttach
    , createNamespacedBindingBinding
    , connectGetNamespacedPodExec
    , connectPostNamespacedPodExec
    , readNamespacedPodLog
    , connectGetNamespacedPodPortforward
    , connectPostNamespacedPodPortforward
    , connectGetNamespacedPodProxy
    -- , connectHeadNamespacedPodProxy
    , connectPutNamespacedPodProxy
    , connectPostNamespacedPodProxy
    , connectDeleteNamespacedPodProxy
    -- , connectOptionsNamespacedPodProxy
    , connectGetNamespacedPodProxy_0
    -- , connectHeadNamespacedPodProxy_0
    , connectPutNamespacedPodProxy_0
    , connectPostNamespacedPodProxy_0
    , connectDeleteNamespacedPodProxy_0
    -- , connectOptionsNamespacedPodProxy_0
    , replaceNamespacedPodStatus
    , listNamespacedPodTemplate
    , createNamespacedPodTemplate
    , deletecollectionNamespacedPodTemplate
    , readNamespacedPodTemplate
    , replaceNamespacedPodTemplate
    , deleteNamespacedPodTemplate
    , patchNamespacedPodTemplate
    , listNamespacedReplicationController
    , createNamespacedReplicationController
    , deletecollectionNamespacedReplicationController
    , readNamespacedReplicationController
    , replaceNamespacedReplicationController
    , deleteNamespacedReplicationController
    , patchNamespacedReplicationController
    , replaceNamespacedReplicationControllerStatus
    , listNamespacedResourceQuota
    , createNamespacedResourceQuota
    , deletecollectionNamespacedResourceQuota
    , readNamespacedResourceQuota
    , replaceNamespacedResourceQuota
    , deleteNamespacedResourceQuota
    , patchNamespacedResourceQuota
    , replaceNamespacedResourceQuotaStatus
    , listNamespacedSecret
    , createNamespacedSecret
    , deletecollectionNamespacedSecret
    , readNamespacedSecret
    , replaceNamespacedSecret
    , deleteNamespacedSecret
    , patchNamespacedSecret
    , listNamespacedServiceAccount
    , createNamespacedServiceAccount
    , deletecollectionNamespacedServiceAccount
    , readNamespacedServiceAccount
    , replaceNamespacedServiceAccount
    , deleteNamespacedServiceAccount
    , patchNamespacedServiceAccount
    , listNamespacedService
    , createNamespacedService
    , readNamespacedService
    , replaceNamespacedService
    , deleteNamespacedService
    , patchNamespacedService
    , replaceNamespacedServiceStatus
    , readNamespacedNamespace
    , replaceNamespacedNamespace
    , deleteNamespacedNamespace
    , patchNamespacedNamespace
    , replaceNamespacedNamespaceFinalize
    , replaceNamespacedNamespaceStatus
    , listNamespacedNode
    , createNamespacedNode
    , deletecollectionNamespacedNode
    , readNamespacedNode
    , replaceNamespacedNode
    , deleteNamespacedNode
    , patchNamespacedNode
    , replaceNamespacedNodeStatus
    , listPersistentVolumeClaim
    , listNamespacedPersistentVolume
    , createNamespacedPersistentVolume
    , deletecollectionNamespacedPersistentVolume
    , readNamespacedPersistentVolume
    , replaceNamespacedPersistentVolume
    , deleteNamespacedPersistentVolume
    , patchNamespacedPersistentVolume
    , replaceNamespacedPersistentVolumeStatus
    , listPod
    , listPodTemplate
    , proxyGETNamespacedPod
    -- , proxyHEADNamespacedPod
    , proxyPUTNamespacedPod
    , proxyPOSTNamespacedPod
    , proxyDELETENamespacedPod
    -- , proxyOPTIONSNamespacedPod
    , proxyGETNamespacedPod_0
    -- , proxyHEADNamespacedPod_0
    , proxyPUTNamespacedPod_0
    , proxyPOSTNamespacedPod_0
    , proxyDELETENamespacedPod_0
    -- , proxyOPTIONSNamespacedPod_0
    , proxyGETNamespacedService
    -- , proxyHEADNamespacedService
    , proxyPUTNamespacedService
    , proxyPOSTNamespacedService
    , proxyDELETENamespacedService
    -- , proxyOPTIONSNamespacedService
    , proxyGETNamespacedService_0
    -- , proxyHEADNamespacedService_0
    , proxyPUTNamespacedService_0
    , proxyPOSTNamespacedService_0
    , proxyDELETENamespacedService_0
    -- , proxyOPTIONSNamespacedService_0
    , proxyGETNamespacedNode
    -- , proxyHEADNamespacedNode
    , proxyPUTNamespacedNode
    , proxyPOSTNamespacedNode
    , proxyDELETENamespacedNode
    -- , proxyOPTIONSNamespacedNode
    , proxyGETNamespacedNode_0
    -- , proxyHEADNamespacedNode_0
    , proxyPUTNamespacedNode_0
    , proxyPOSTNamespacedNode_0
    , proxyDELETENamespacedNode_0
    -- , proxyOPTIONSNamespacedNode_0
    , listReplicationController
    , listResourceQuota
    , listSecret
    , listServiceAccount
    , listService
    , watchConfigMapList
    , watchEndpointsList
    , watchEventList
    , watchLimitRangeList
    , watchNamespacedNamespaceList
    , watchNamespacedConfigMapList
    , watchNamespacedConfigMap
    , watchNamespacedEndpointsList
    , watchNamespacedEndpoints
    , watchNamespacedEventList
    , watchNamespacedEvent
    , watchNamespacedLimitRangeList
    , watchNamespacedLimitRange
    , watchNamespacedPersistentVolumeClaimList
    , watchNamespacedPersistentVolumeClaim
    , watchNamespacedPodList
    , watchNamespacedPod
    , watchNamespacedPodTemplateList
    , watchNamespacedPodTemplate
    , watchNamespacedReplicationControllerList
    , watchNamespacedReplicationController
    , watchNamespacedResourceQuotaList
    , watchNamespacedResourceQuota
    , watchNamespacedSecretList
    , watchNamespacedSecret
    , watchNamespacedServiceAccountList
    , watchNamespacedServiceAccount
    , watchNamespacedServiceList
    , watchNamespacedService
    , watchNamespacedNamespace
    , watchNamespacedNodeList
    , watchNamespacedNode
    , watchPersistentVolumeClaimList
    , watchNamespacedPersistentVolumeList
    , watchNamespacedPersistentVolume
    , watchPodList
    , watchPodTemplateList
    , watchReplicationControllerList
    , watchResourceQuotaList
    , watchSecretList
    , watchServiceAccountList
    , watchServiceList
    , proxyApivApi
    , ApivApi
    ) where

import GHC.Generics
import Data.Proxy
import Servant.API
import Servant.Client
import Network.URI (URI (..), URIAuth (..), parseURI)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Servant.Common.Text
import Data.List (intercalate)
import qualified Data.Text as T
import Test.QuickCheck
import Kubernetes.Model.V1.ComponentStatusList
import Kubernetes.Model.V1.ComponentStatus
import Kubernetes.Model.V1.ConfigMapList
import Kubernetes.Model.V1.EndpointsList
import Kubernetes.Model.V1.EventList
import Kubernetes.Model.V1.LimitRangeList
import Kubernetes.Model.V1.NamespaceList
import Kubernetes.Model.V1.Namespace
import Kubernetes.Model.Unversioned.Status
import Kubernetes.Model.V1.Binding
import Kubernetes.Model.V1.ConfigMap
import Kubernetes.Model.V1.DeleteOptions
import qualified Kubernetes.Model.Unversioned.Patch as KubePatch
import Kubernetes.Model.V1.Endpoints
import Kubernetes.Model.V1.Event
import Kubernetes.Model.V1.LimitRange
import Kubernetes.Model.V1.PersistentVolumeClaimList
import Kubernetes.Model.V1.PersistentVolumeClaim
import Kubernetes.Model.V1.PodList
import Kubernetes.Model.V1.Pod
import Kubernetes.Model.V1.PodTemplateList
import Kubernetes.Model.V1.PodTemplate
import Kubernetes.Model.V1.ReplicationControllerList
import Kubernetes.Model.V1.ReplicationController
import Kubernetes.Model.V1.ResourceQuotaList
import Kubernetes.Model.V1.ResourceQuota
import Kubernetes.Model.V1.SecretList
import Kubernetes.Model.V1.Secret
import Kubernetes.Model.V1.ServiceAccountList
import Kubernetes.Model.V1.ServiceAccount
import Kubernetes.Model.V1.ServiceList
import Kubernetes.Model.V1.Service
import Kubernetes.Model.V1.NodeList
import Kubernetes.Model.V1.Node
import Kubernetes.Model.V1.PersistentVolumeList
import Kubernetes.Model.V1.PersistentVolume
import Kubernetes.Model.Json.WatchEvent
import Kubernetes.Utils

type ApivApi = "api" :> "v1" :> Get '[JSON] () -- getAPIResources
    :<|> "api" :> "v1" :> "componentstatuses" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ComponentStatusList -- listNamespacedComponentStatus
    :<|> "api" :> "v1" :> "componentstatuses" :> Capture "name" Text :> QueryParam "pretty" Text :> Get '[JSON] ComponentStatus -- readNamespacedComponentStatus
    :<|> "api" :> "v1" :> "configmaps" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ConfigMapList -- listConfigMap
    :<|> "api" :> "v1" :> "endpoints" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] EndpointsList -- listEndpoints
    :<|> "api" :> "v1" :> "events" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] EventList -- listEvent
    :<|> "api" :> "v1" :> "limitranges" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] LimitRangeList -- listLimitRange
    :<|> "api" :> "v1" :> "namespaces" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] NamespaceList -- listNamespacedNamespace
    :<|> "api" :> "v1" :> "namespaces" :> QueryParam "pretty" Text :> ReqBody '[JSON] Namespace :> Post '[JSON] Namespace -- createNamespacedNamespace
    :<|> "api" :> "v1" :> "namespaces" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedNamespace
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "bindings" :> QueryParam "pretty" Text :> ReqBody '[JSON] Binding :> Post '[JSON] Binding -- createNamespacedBinding
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ConfigMapList -- listNamespacedConfigMap
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> QueryParam "pretty" Text :> ReqBody '[JSON] ConfigMap :> Post '[JSON] ConfigMap -- createNamespacedConfigMap
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedConfigMap
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] ConfigMap -- readNamespacedConfigMap
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] ConfigMap :> Put '[JSON] ConfigMap -- replaceNamespacedConfigMap
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedConfigMap
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] ConfigMap -- patchNamespacedConfigMap
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] EndpointsList -- listNamespacedEndpoints
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> QueryParam "pretty" Text :> ReqBody '[JSON] Endpoints :> Post '[JSON] Endpoints -- createNamespacedEndpoints
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedEndpoints
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] Endpoints -- readNamespacedEndpoints
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] Endpoints :> Put '[JSON] Endpoints -- replaceNamespacedEndpoints
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedEndpoints
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] Endpoints -- patchNamespacedEndpoints
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "events" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] EventList -- listNamespacedEvent
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "events" :> QueryParam "pretty" Text :> ReqBody '[JSON] Event :> Post '[JSON] Event -- createNamespacedEvent
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "events" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedEvent
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "events" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] Event -- readNamespacedEvent
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "events" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] Event :> Put '[JSON] Event -- replaceNamespacedEvent
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "events" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedEvent
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "events" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] Event -- patchNamespacedEvent
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] LimitRangeList -- listNamespacedLimitRange
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> QueryParam "pretty" Text :> ReqBody '[JSON] LimitRange :> Post '[JSON] LimitRange -- createNamespacedLimitRange
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedLimitRange
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] LimitRange -- readNamespacedLimitRange
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] LimitRange :> Put '[JSON] LimitRange -- replaceNamespacedLimitRange
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedLimitRange
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] LimitRange -- patchNamespacedLimitRange
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] PersistentVolumeClaimList -- listNamespacedPersistentVolumeClaim
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> QueryParam "pretty" Text :> ReqBody '[JSON] PersistentVolumeClaim :> Post '[JSON] PersistentVolumeClaim -- createNamespacedPersistentVolumeClaim
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedPersistentVolumeClaim
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] PersistentVolumeClaim -- readNamespacedPersistentVolumeClaim
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] PersistentVolumeClaim :> Put '[JSON] PersistentVolumeClaim -- replaceNamespacedPersistentVolumeClaim
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedPersistentVolumeClaim
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] PersistentVolumeClaim -- patchNamespacedPersistentVolumeClaim
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> Capture "name" Text :> "status" :> QueryParam "pretty" Text :> ReqBody '[JSON] PersistentVolumeClaim :> Put '[JSON] PersistentVolumeClaim -- replaceNamespacedPersistentVolumeClaimStatus
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] PodList -- listNamespacedPod
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> QueryParam "pretty" Text :> ReqBody '[JSON] Pod :> Post '[JSON] Pod -- createNamespacedPod
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedPod
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] Pod -- readNamespacedPod
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] Pod :> Put '[JSON] Pod -- replaceNamespacedPod
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedPod
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] Pod -- patchNamespacedPod
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "attach" :> QueryParam "stdin" Bool :> QueryParam "stdout" Bool :> QueryParam "stderr" Bool :> QueryParam "tty" Bool :> QueryParam "container" Text :> Get '[JSON] Text -- connectGetNamespacedPodAttach
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "attach" :> QueryParam "stdin" Bool :> QueryParam "stdout" Bool :> QueryParam "stderr" Bool :> QueryParam "tty" Bool :> QueryParam "container" Text :> Post '[JSON] Text -- connectPostNamespacedPodAttach
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "binding" :> QueryParam "pretty" Text :> ReqBody '[JSON] Binding :> Post '[JSON] Binding -- createNamespacedBindingBinding
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "exec" :> QueryParam "stdin" Bool :> QueryParam "stdout" Bool :> QueryParam "stderr" Bool :> QueryParam "tty" Bool :> QueryParam "container" Text :> QueryParam "command" Text :> Get '[JSON] Text -- connectGetNamespacedPodExec
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "exec" :> QueryParam "stdin" Bool :> QueryParam "stdout" Bool :> QueryParam "stderr" Bool :> QueryParam "tty" Bool :> QueryParam "container" Text :> QueryParam "command" Text :> Post '[JSON] Text -- connectPostNamespacedPodExec
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "log" :> QueryParam "pretty" Text :> QueryParam "container" Text :> QueryParam "follow" Bool :> QueryParam "previous" Bool :> QueryParam "sinceSeconds" Integer :> QueryParam "sinceTime" Text :> QueryParam "timestamps" Bool :> QueryParam "tailLines" Integer :> QueryParam "limitBytes" Integer :> Get '[JSON] Pod -- readNamespacedPodLog
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "portforward" :> Get '[JSON] Text -- connectGetNamespacedPodPortforward
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "portforward" :> Post '[JSON] Text -- connectPostNamespacedPodPortforward
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> QueryParam "path" Text :> Get '[JSON] Text -- connectGetNamespacedPodProxy
    -- :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> QueryParam "path" Text :> Head '[JSON] Text -- connectHeadNamespacedPodProxy
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> QueryParam "path" Text :> Put '[JSON] Text -- connectPutNamespacedPodProxy
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> QueryParam "path" Text :> Post '[JSON] Text -- connectPostNamespacedPodProxy
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> QueryParam "path" Text :> Delete '[JSON] Text -- connectDeleteNamespacedPodProxy
    -- :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> QueryParam "path" Text :> Options '[JSON] Text -- connectOptionsNamespacedPodProxy
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> Capture "path" Text :> QueryParam "path" Text :> Get '[JSON] Text -- connectGetNamespacedPodProxy_0
    -- :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> Capture "path" Text :> QueryParam "path" Text :> Head '[JSON] Text -- connectHeadNamespacedPodProxy_0
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> Capture "path" Text :> QueryParam "path" Text :> Put '[JSON] Text -- connectPutNamespacedPodProxy_0
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> Capture "path" Text :> QueryParam "path" Text :> Post '[JSON] Text -- connectPostNamespacedPodProxy_0
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> Capture "path" Text :> QueryParam "path" Text :> Delete '[JSON] Text -- connectDeleteNamespacedPodProxy_0
    -- :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "proxy" :> Capture "path" Text :> QueryParam "path" Text :> Options '[JSON] Text -- connectOptionsNamespacedPodProxy_0
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> "status" :> QueryParam "pretty" Text :> ReqBody '[JSON] Pod :> Put '[JSON] Pod -- replaceNamespacedPodStatus
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] PodTemplateList -- listNamespacedPodTemplate
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> QueryParam "pretty" Text :> ReqBody '[JSON] PodTemplate :> Post '[JSON] PodTemplate -- createNamespacedPodTemplate
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedPodTemplate
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] PodTemplate -- readNamespacedPodTemplate
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] PodTemplate :> Put '[JSON] PodTemplate -- replaceNamespacedPodTemplate
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedPodTemplate
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] PodTemplate -- patchNamespacedPodTemplate
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ReplicationControllerList -- listNamespacedReplicationController
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> QueryParam "pretty" Text :> ReqBody '[JSON] ReplicationController :> Post '[JSON] ReplicationController -- createNamespacedReplicationController
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedReplicationController
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] ReplicationController -- readNamespacedReplicationController
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] ReplicationController :> Put '[JSON] ReplicationController -- replaceNamespacedReplicationController
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedReplicationController
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] ReplicationController -- patchNamespacedReplicationController
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> Capture "name" Text :> "status" :> QueryParam "pretty" Text :> ReqBody '[JSON] ReplicationController :> Put '[JSON] ReplicationController -- replaceNamespacedReplicationControllerStatus
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ResourceQuotaList -- listNamespacedResourceQuota
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> QueryParam "pretty" Text :> ReqBody '[JSON] ResourceQuota :> Post '[JSON] ResourceQuota -- createNamespacedResourceQuota
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedResourceQuota
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] ResourceQuota -- readNamespacedResourceQuota
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] ResourceQuota :> Put '[JSON] ResourceQuota -- replaceNamespacedResourceQuota
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedResourceQuota
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] ResourceQuota -- patchNamespacedResourceQuota
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> Capture "name" Text :> "status" :> QueryParam "pretty" Text :> ReqBody '[JSON] ResourceQuota :> Put '[JSON] ResourceQuota -- replaceNamespacedResourceQuotaStatus
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] SecretList -- listNamespacedSecret
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> QueryParam "pretty" Text :> ReqBody '[JSON] Secret :> Post '[JSON] Secret -- createNamespacedSecret
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedSecret
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] Secret -- readNamespacedSecret
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] Secret :> Put '[JSON] Secret -- replaceNamespacedSecret
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedSecret
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] Secret -- patchNamespacedSecret
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ServiceAccountList -- listNamespacedServiceAccount
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> QueryParam "pretty" Text :> ReqBody '[JSON] ServiceAccount :> Post '[JSON] ServiceAccount -- createNamespacedServiceAccount
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedServiceAccount
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] ServiceAccount -- readNamespacedServiceAccount
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] ServiceAccount :> Put '[JSON] ServiceAccount -- replaceNamespacedServiceAccount
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedServiceAccount
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] ServiceAccount -- patchNamespacedServiceAccount
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "services" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ServiceList -- listNamespacedService
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "services" :> QueryParam "pretty" Text :> ReqBody '[JSON] Service :> Post '[JSON] Service -- createNamespacedService
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> QueryParam "pretty" Text :> Get '[JSON] Service -- readNamespacedService
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] Service :> Put '[JSON] Service -- replaceNamespacedService
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> QueryParam "pretty" Text :> Delete '[JSON] Status -- deleteNamespacedService
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] Service -- patchNamespacedService
    :<|> "api" :> "v1" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> "status" :> QueryParam "pretty" Text :> ReqBody '[JSON] Service :> Put '[JSON] Service -- replaceNamespacedServiceStatus
    :<|> "api" :> "v1" :> "namespaces" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] Namespace -- readNamespacedNamespace
    :<|> "api" :> "v1" :> "namespaces" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] Namespace :> Put '[JSON] Namespace -- replaceNamespacedNamespace
    :<|> "api" :> "v1" :> "namespaces" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedNamespace
    :<|> "api" :> "v1" :> "namespaces" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] Namespace -- patchNamespacedNamespace
    :<|> "api" :> "v1" :> "namespaces" :> Capture "name" Text :> "finalize" :> QueryParam "pretty" Text :> ReqBody '[JSON] Namespace :> Put '[JSON] Namespace -- replaceNamespacedNamespaceFinalize
    :<|> "api" :> "v1" :> "namespaces" :> Capture "name" Text :> "status" :> QueryParam "pretty" Text :> ReqBody '[JSON] Namespace :> Put '[JSON] Namespace -- replaceNamespacedNamespaceStatus
    :<|> "api" :> "v1" :> "nodes" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] NodeList -- listNamespacedNode
    :<|> "api" :> "v1" :> "nodes" :> QueryParam "pretty" Text :> ReqBody '[JSON] Node :> Post '[JSON] Node -- createNamespacedNode
    :<|> "api" :> "v1" :> "nodes" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedNode
    :<|> "api" :> "v1" :> "nodes" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] Node -- readNamespacedNode
    :<|> "api" :> "v1" :> "nodes" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] Node :> Put '[JSON] Node -- replaceNamespacedNode
    :<|> "api" :> "v1" :> "nodes" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedNode
    :<|> "api" :> "v1" :> "nodes" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] Node -- patchNamespacedNode
    :<|> "api" :> "v1" :> "nodes" :> Capture "name" Text :> "status" :> QueryParam "pretty" Text :> ReqBody '[JSON] Node :> Put '[JSON] Node -- replaceNamespacedNodeStatus
    :<|> "api" :> "v1" :> "persistentvolumeclaims" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] PersistentVolumeClaimList -- listPersistentVolumeClaim
    :<|> "api" :> "v1" :> "persistentvolumes" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] PersistentVolumeList -- listNamespacedPersistentVolume
    :<|> "api" :> "v1" :> "persistentvolumes" :> QueryParam "pretty" Text :> ReqBody '[JSON] PersistentVolume :> Post '[JSON] PersistentVolume -- createNamespacedPersistentVolume
    :<|> "api" :> "v1" :> "persistentvolumes" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Delete '[JSON] Status -- deletecollectionNamespacedPersistentVolume
    :<|> "api" :> "v1" :> "persistentvolumes" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "export" Bool :> QueryParam "exact" Bool :> Get '[JSON] PersistentVolume -- readNamespacedPersistentVolume
    :<|> "api" :> "v1" :> "persistentvolumes" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] PersistentVolume :> Put '[JSON] PersistentVolume -- replaceNamespacedPersistentVolume
    :<|> "api" :> "v1" :> "persistentvolumes" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] DeleteOptions :> Delete '[JSON] Status -- deleteNamespacedPersistentVolume
    :<|> "api" :> "v1" :> "persistentvolumes" :> Capture "name" Text :> QueryParam "pretty" Text :> ReqBody '[JSON] KubePatch.Patch :> Patch '[JSON] PersistentVolume -- patchNamespacedPersistentVolume
    :<|> "api" :> "v1" :> "persistentvolumes" :> Capture "name" Text :> "status" :> QueryParam "pretty" Text :> ReqBody '[JSON] PersistentVolume :> Put '[JSON] PersistentVolume -- replaceNamespacedPersistentVolumeStatus
    :<|> "api" :> "v1" :> "pods" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] PodList -- listPod
    :<|> "api" :> "v1" :> "podtemplates" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] PodTemplateList -- listPodTemplate
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Get '[JSON] Text -- proxyGETNamespacedPod
    -- :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Head '[JSON] Text -- proxyHEADNamespacedPod
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Put '[JSON] Text -- proxyPUTNamespacedPod
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Post '[JSON] Text -- proxyPOSTNamespacedPod
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Delete '[JSON] Text -- proxyDELETENamespacedPod
    -- :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Options '[JSON] Text -- proxyOPTIONSNamespacedPod
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Capture "path" Text :> Get '[JSON] Text -- proxyGETNamespacedPod_0
    -- :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Capture "path" Text :> Head '[JSON] Text -- proxyHEADNamespacedPod_0
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Capture "path" Text :> Put '[JSON] Text -- proxyPUTNamespacedPod_0
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Capture "path" Text :> Post '[JSON] Text -- proxyPOSTNamespacedPod_0
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Capture "path" Text :> Delete '[JSON] Text -- proxyDELETENamespacedPod_0
    -- :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> Capture "path" Text :> Options '[JSON] Text -- proxyOPTIONSNamespacedPod_0
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Get '[JSON] Text -- proxyGETNamespacedService
    -- :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Head '[JSON] Text -- proxyHEADNamespacedService
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Put '[JSON] Text -- proxyPUTNamespacedService
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Post '[JSON] Text -- proxyPOSTNamespacedService
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Delete '[JSON] Text -- proxyDELETENamespacedService
    -- :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Options '[JSON] Text -- proxyOPTIONSNamespacedService
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Capture "path" Text :> Get '[JSON] Text -- proxyGETNamespacedService_0
    -- :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Capture "path" Text :> Head '[JSON] Text -- proxyHEADNamespacedService_0
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Capture "path" Text :> Put '[JSON] Text -- proxyPUTNamespacedService_0
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Capture "path" Text :> Post '[JSON] Text -- proxyPOSTNamespacedService_0
    :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Capture "path" Text :> Delete '[JSON] Text -- proxyDELETENamespacedService_0
    -- :<|> "api" :> "v1" :> "proxy" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> Capture "path" Text :> Options '[JSON] Text -- proxyOPTIONSNamespacedService_0
    :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Get '[JSON] Text -- proxyGETNamespacedNode
    -- :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Head '[JSON] Text -- proxyHEADNamespacedNode
    :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Put '[JSON] Text -- proxyPUTNamespacedNode
    :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Post '[JSON] Text -- proxyPOSTNamespacedNode
    :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Delete '[JSON] Text -- proxyDELETENamespacedNode
    -- :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Options '[JSON] Text -- proxyOPTIONSNamespacedNode
    :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Capture "path" Text :> Get '[JSON] Text -- proxyGETNamespacedNode_0
    -- :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Capture "path" Text :> Head '[JSON] Text -- proxyHEADNamespacedNode_0
    :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Capture "path" Text :> Put '[JSON] Text -- proxyPUTNamespacedNode_0
    :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Capture "path" Text :> Post '[JSON] Text -- proxyPOSTNamespacedNode_0
    :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Capture "path" Text :> Delete '[JSON] Text -- proxyDELETENamespacedNode_0
    -- :<|> "api" :> "v1" :> "proxy" :> "nodes" :> Capture "name" Text :> Capture "path" Text :> Options '[JSON] Text -- proxyOPTIONSNamespacedNode_0
    :<|> "api" :> "v1" :> "replicationcontrollers" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ReplicationControllerList -- listReplicationController
    :<|> "api" :> "v1" :> "resourcequotas" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ResourceQuotaList -- listResourceQuota
    :<|> "api" :> "v1" :> "secrets" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] SecretList -- listSecret
    :<|> "api" :> "v1" :> "serviceaccounts" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ServiceAccountList -- listServiceAccount
    :<|> "api" :> "v1" :> "services" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] ServiceList -- listService
    :<|> "api" :> "v1" :> "watch" :> "configmaps" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchConfigMapList
    :<|> "api" :> "v1" :> "watch" :> "endpoints" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchEndpointsList
    :<|> "api" :> "v1" :> "watch" :> "events" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchEventList
    :<|> "api" :> "v1" :> "watch" :> "limitranges" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchLimitRangeList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedNamespaceList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedConfigMapList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "configmaps" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedConfigMap
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedEndpointsList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "endpoints" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedEndpoints
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "events" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedEventList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "events" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedEvent
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedLimitRangeList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "limitranges" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedLimitRange
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedPersistentVolumeClaimList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "persistentvolumeclaims" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedPersistentVolumeClaim
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "pods" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedPodList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "pods" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedPod
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedPodTemplateList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "podtemplates" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedPodTemplate
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedReplicationControllerList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "replicationcontrollers" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedReplicationController
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedResourceQuotaList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "resourcequotas" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedResourceQuota
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedSecretList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "secrets" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedSecret
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedServiceAccountList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "serviceaccounts" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedServiceAccount
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "services" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedServiceList
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "namespace" Text :> "services" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedService
    :<|> "api" :> "v1" :> "watch" :> "namespaces" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedNamespace
    :<|> "api" :> "v1" :> "watch" :> "nodes" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedNodeList
    :<|> "api" :> "v1" :> "watch" :> "nodes" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedNode
    :<|> "api" :> "v1" :> "watch" :> "persistentvolumeclaims" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchPersistentVolumeClaimList
    :<|> "api" :> "v1" :> "watch" :> "persistentvolumes" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedPersistentVolumeList
    :<|> "api" :> "v1" :> "watch" :> "persistentvolumes" :> Capture "name" Text :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchNamespacedPersistentVolume
    :<|> "api" :> "v1" :> "watch" :> "pods" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchPodList
    :<|> "api" :> "v1" :> "watch" :> "podtemplates" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchPodTemplateList
    :<|> "api" :> "v1" :> "watch" :> "replicationcontrollers" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchReplicationControllerList
    :<|> "api" :> "v1" :> "watch" :> "resourcequotas" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchResourceQuotaList
    :<|> "api" :> "v1" :> "watch" :> "secrets" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchSecretList
    :<|> "api" :> "v1" :> "watch" :> "serviceaccounts" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchServiceAccountList
    :<|> "api" :> "v1" :> "watch" :> "services" :> QueryParam "pretty" Text :> QueryParam "labelSelector" Text :> QueryParam "fieldSelector" Text :> QueryParam "watch" Bool :> QueryParam "resourceVersion" Text :> QueryParam "timeoutSeconds" Integer :> Get '[JSON] WatchEvent -- watchServiceList

proxyApivApi :: Proxy ApivApi
proxyApivApi = Proxy


serverPath :: String
serverPath = "https://10.10.10.10:6443/"

parseHostPort :: String -> (String, Int)
parseHostPort path = (host,port)
    where
        authority = case parseURI path of
            Just x -> uriAuthority x
            _      -> Nothing
        (host, port) = case authority of
            Just y -> (uriRegName y, (getPort . uriPort) y)
            _      -> ("localhost", 8080)
        getPort p = case (length p) of
            0 -> 80
            _ -> (read . drop 1) p

(host, port) = parseHostPort serverPath

getAPIResources
    :<|> listNamespacedComponentStatus
    :<|> readNamespacedComponentStatus
    :<|> listConfigMap
    :<|> listEndpoints
    :<|> listEvent
    :<|> listLimitRange
    :<|> listNamespacedNamespace
    :<|> createNamespacedNamespace
    :<|> deletecollectionNamespacedNamespace
    :<|> createNamespacedBinding
    :<|> listNamespacedConfigMap
    :<|> createNamespacedConfigMap
    :<|> deletecollectionNamespacedConfigMap
    :<|> readNamespacedConfigMap
    :<|> replaceNamespacedConfigMap
    :<|> deleteNamespacedConfigMap
    :<|> patchNamespacedConfigMap
    :<|> listNamespacedEndpoints
    :<|> createNamespacedEndpoints
    :<|> deletecollectionNamespacedEndpoints
    :<|> readNamespacedEndpoints
    :<|> replaceNamespacedEndpoints
    :<|> deleteNamespacedEndpoints
    :<|> patchNamespacedEndpoints
    :<|> listNamespacedEvent
    :<|> createNamespacedEvent
    :<|> deletecollectionNamespacedEvent
    :<|> readNamespacedEvent
    :<|> replaceNamespacedEvent
    :<|> deleteNamespacedEvent
    :<|> patchNamespacedEvent
    :<|> listNamespacedLimitRange
    :<|> createNamespacedLimitRange
    :<|> deletecollectionNamespacedLimitRange
    :<|> readNamespacedLimitRange
    :<|> replaceNamespacedLimitRange
    :<|> deleteNamespacedLimitRange
    :<|> patchNamespacedLimitRange
    :<|> listNamespacedPersistentVolumeClaim
    :<|> createNamespacedPersistentVolumeClaim
    :<|> deletecollectionNamespacedPersistentVolumeClaim
    :<|> readNamespacedPersistentVolumeClaim
    :<|> replaceNamespacedPersistentVolumeClaim
    :<|> deleteNamespacedPersistentVolumeClaim
    :<|> patchNamespacedPersistentVolumeClaim
    :<|> replaceNamespacedPersistentVolumeClaimStatus
    :<|> listNamespacedPod
    :<|> createNamespacedPod
    :<|> deletecollectionNamespacedPod
    :<|> readNamespacedPod
    :<|> replaceNamespacedPod
    :<|> deleteNamespacedPod
    :<|> patchNamespacedPod
    :<|> connectGetNamespacedPodAttach
    :<|> connectPostNamespacedPodAttach
    :<|> createNamespacedBindingBinding
    :<|> connectGetNamespacedPodExec
    :<|> connectPostNamespacedPodExec
    :<|> readNamespacedPodLog
    :<|> connectGetNamespacedPodPortforward
    :<|> connectPostNamespacedPodPortforward
    :<|> connectGetNamespacedPodProxy
    -- :<|> connectHeadNamespacedPodProxy
    :<|> connectPutNamespacedPodProxy
    :<|> connectPostNamespacedPodProxy
    :<|> connectDeleteNamespacedPodProxy
    -- :<|> connectOptionsNamespacedPodProxy
    :<|> connectGetNamespacedPodProxy_0
    -- :<|> connectHeadNamespacedPodProxy_0
    :<|> connectPutNamespacedPodProxy_0
    :<|> connectPostNamespacedPodProxy_0
    :<|> connectDeleteNamespacedPodProxy_0
    -- :<|> connectOptionsNamespacedPodProxy_0
    :<|> replaceNamespacedPodStatus
    :<|> listNamespacedPodTemplate
    :<|> createNamespacedPodTemplate
    :<|> deletecollectionNamespacedPodTemplate
    :<|> readNamespacedPodTemplate
    :<|> replaceNamespacedPodTemplate
    :<|> deleteNamespacedPodTemplate
    :<|> patchNamespacedPodTemplate
    :<|> listNamespacedReplicationController
    :<|> createNamespacedReplicationController
    :<|> deletecollectionNamespacedReplicationController
    :<|> readNamespacedReplicationController
    :<|> replaceNamespacedReplicationController
    :<|> deleteNamespacedReplicationController
    :<|> patchNamespacedReplicationController
    :<|> replaceNamespacedReplicationControllerStatus
    :<|> listNamespacedResourceQuota
    :<|> createNamespacedResourceQuota
    :<|> deletecollectionNamespacedResourceQuota
    :<|> readNamespacedResourceQuota
    :<|> replaceNamespacedResourceQuota
    :<|> deleteNamespacedResourceQuota
    :<|> patchNamespacedResourceQuota
    :<|> replaceNamespacedResourceQuotaStatus
    :<|> listNamespacedSecret
    :<|> createNamespacedSecret
    :<|> deletecollectionNamespacedSecret
    :<|> readNamespacedSecret
    :<|> replaceNamespacedSecret
    :<|> deleteNamespacedSecret
    :<|> patchNamespacedSecret
    :<|> listNamespacedServiceAccount
    :<|> createNamespacedServiceAccount
    :<|> deletecollectionNamespacedServiceAccount
    :<|> readNamespacedServiceAccount
    :<|> replaceNamespacedServiceAccount
    :<|> deleteNamespacedServiceAccount
    :<|> patchNamespacedServiceAccount
    :<|> listNamespacedService
    :<|> createNamespacedService
    :<|> readNamespacedService
    :<|> replaceNamespacedService
    :<|> deleteNamespacedService
    :<|> patchNamespacedService
    :<|> replaceNamespacedServiceStatus
    :<|> readNamespacedNamespace
    :<|> replaceNamespacedNamespace
    :<|> deleteNamespacedNamespace
    :<|> patchNamespacedNamespace
    :<|> replaceNamespacedNamespaceFinalize
    :<|> replaceNamespacedNamespaceStatus
    :<|> listNamespacedNode
    :<|> createNamespacedNode
    :<|> deletecollectionNamespacedNode
    :<|> readNamespacedNode
    :<|> replaceNamespacedNode
    :<|> deleteNamespacedNode
    :<|> patchNamespacedNode
    :<|> replaceNamespacedNodeStatus
    :<|> listPersistentVolumeClaim
    :<|> listNamespacedPersistentVolume
    :<|> createNamespacedPersistentVolume
    :<|> deletecollectionNamespacedPersistentVolume
    :<|> readNamespacedPersistentVolume
    :<|> replaceNamespacedPersistentVolume
    :<|> deleteNamespacedPersistentVolume
    :<|> patchNamespacedPersistentVolume
    :<|> replaceNamespacedPersistentVolumeStatus
    :<|> listPod
    :<|> listPodTemplate
    :<|> proxyGETNamespacedPod
    -- :<|> proxyHEADNamespacedPod
    :<|> proxyPUTNamespacedPod
    :<|> proxyPOSTNamespacedPod
    :<|> proxyDELETENamespacedPod
    -- :<|> proxyOPTIONSNamespacedPod
    :<|> proxyGETNamespacedPod_0
    -- :<|> proxyHEADNamespacedPod_0
    :<|> proxyPUTNamespacedPod_0
    :<|> proxyPOSTNamespacedPod_0
    :<|> proxyDELETENamespacedPod_0
    -- :<|> proxyOPTIONSNamespacedPod_0
    :<|> proxyGETNamespacedService
    -- :<|> proxyHEADNamespacedService
    :<|> proxyPUTNamespacedService
    :<|> proxyPOSTNamespacedService
    :<|> proxyDELETENamespacedService
    -- :<|> proxyOPTIONSNamespacedService
    :<|> proxyGETNamespacedService_0
    -- :<|> proxyHEADNamespacedService_0
    :<|> proxyPUTNamespacedService_0
    :<|> proxyPOSTNamespacedService_0
    :<|> proxyDELETENamespacedService_0
    -- :<|> proxyOPTIONSNamespacedService_0
    :<|> proxyGETNamespacedNode
    -- :<|> proxyHEADNamespacedNode
    :<|> proxyPUTNamespacedNode
    :<|> proxyPOSTNamespacedNode
    :<|> proxyDELETENamespacedNode
    -- :<|> proxyOPTIONSNamespacedNode
    :<|> proxyGETNamespacedNode_0
    -- :<|> proxyHEADNamespacedNode_0
    :<|> proxyPUTNamespacedNode_0
    :<|> proxyPOSTNamespacedNode_0
    :<|> proxyDELETENamespacedNode_0
    -- :<|> proxyOPTIONSNamespacedNode_0
    :<|> listReplicationController
    :<|> listResourceQuota
    :<|> listSecret
    :<|> listServiceAccount
    :<|> listService
    :<|> watchConfigMapList
    :<|> watchEndpointsList
    :<|> watchEventList
    :<|> watchLimitRangeList
    :<|> watchNamespacedNamespaceList
    :<|> watchNamespacedConfigMapList
    :<|> watchNamespacedConfigMap
    :<|> watchNamespacedEndpointsList
    :<|> watchNamespacedEndpoints
    :<|> watchNamespacedEventList
    :<|> watchNamespacedEvent
    :<|> watchNamespacedLimitRangeList
    :<|> watchNamespacedLimitRange
    :<|> watchNamespacedPersistentVolumeClaimList
    :<|> watchNamespacedPersistentVolumeClaim
    :<|> watchNamespacedPodList
    :<|> watchNamespacedPod
    :<|> watchNamespacedPodTemplateList
    :<|> watchNamespacedPodTemplate
    :<|> watchNamespacedReplicationControllerList
    :<|> watchNamespacedReplicationController
    :<|> watchNamespacedResourceQuotaList
    :<|> watchNamespacedResourceQuota
    :<|> watchNamespacedSecretList
    :<|> watchNamespacedSecret
    :<|> watchNamespacedServiceAccountList
    :<|> watchNamespacedServiceAccount
    :<|> watchNamespacedServiceList
    :<|> watchNamespacedService
    :<|> watchNamespacedNamespace
    :<|> watchNamespacedNodeList
    :<|> watchNamespacedNode
    :<|> watchPersistentVolumeClaimList
    :<|> watchNamespacedPersistentVolumeList
    :<|> watchNamespacedPersistentVolume
    :<|> watchPodList
    :<|> watchPodTemplateList
    :<|> watchReplicationControllerList
    :<|> watchResourceQuotaList
    :<|> watchSecretList
    :<|> watchServiceAccountList
    :<|> watchServiceList
    = client proxyApivApi $ BaseUrl Http host port
