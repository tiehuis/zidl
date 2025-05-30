/*** Autogenerated by ZIDL 1.0 from anonymous_struct.idl - Do not edit ***/

#ifdef _WIN32
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 475
#endif
#include <rpc.h>
#include <rpcndr.h>
#endif

#ifndef COM_NO_WINDOWS_H
#include <windows.h>
#include <ole2.h>
#endif

#ifndef __anonymous_struct_h__
#define __anonymous_struct_h__

/* Forward declarations */

/* Headers for imported files */


#ifdef __cplusplus
extern "C" {
#endif

typedef struct IKEEXT_CERTIFICATE_AUTHENTICATION0_ {
    IKEEXT_CERT_CONFIG_TYPE inboundConfigType;
    __C89_NAMELESS union {
        __C89_NAMELESS struct {
            UINT32 inboundRootArraySize;
            IKEEXT_CERT_ROOT_CONFIG0 *inboundRootArray;
        } __C89_NAMELESSSTRUCTNAME;
        IKEEXT_CERT_ROOT_CONFIG0 *inboundEnterpriseStoreConfig;
        IKEEXT_CERT_ROOT_CONFIG0 *inboundTrustedRootStoreConfig;
    } __C89_NAMELESSUNIONNAME;
    IKEEXT_CERT_CONFIG_TYPE outboundConfigType;
    __C89_NAMELESS union {
        __C89_NAMELESS struct {
            UINT32 outboundRootArraySize;
            IKEEXT_CERT_ROOT_CONFIG0 *outboundRootArray;
        } __C89_NAMELESSSTRUCTNAME;
        IKEEXT_CERT_ROOT_CONFIG0 *outboundEnterpriseStoreConfig;
        IKEEXT_CERT_ROOT_CONFIG0 *outboundTrustedRootStoreConfig;
    } __C89_NAMELESSUNIONNAME;
    UINT32 flags;
} IKEEXT_CERTIFICATE_AUTHENTICATION0;

/* Begin additional prototypes for all interfaces */


/* End additional prototypes */

#ifdef __cplusplus
}
#endif

#endif /* __anonymous_struct_h__ */
