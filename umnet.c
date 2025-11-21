#include "umka_api.h"
#include "mlibc.h"

#include <string.h>

#ifdef MLIBC_COMPILER_MSVC

#include <windows.h>

BOOL WINAPI DllMain( HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved ) {
	(void)hinstDLL;
	(void)lpvReserved;

	switch ( fdwReason )
	{
	case DLL_PROCESS_ATTACH:
		SocketInitialize();
		break;
	case DLL_THREAD_ATTACH:
		// skip
		break;
	case DLL_THREAD_DETACH:
		// skip
		break;
	case DLL_PROCESS_DETACH:
		SocketRelease();
		break;
	}
	return TRUE;
}

#endif

typedef struct {
	i64 Code;
	str Message;
} UResult;

UMKA_EXPORT void umc_SocketCreate( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	Result const ret = SocketCreate( this );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketClose( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	Result const ret = SocketClose( this );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketBind( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	cstr const address = api->umkaGetParam( p_params, 1 )->ptrVal;
	cstr const port = api->umkaGetParam( p_params, 2 )->ptrVal;
	Result const ret = SocketBind( this, address, port );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketListen( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	i32 const backlog = (i32)api->umkaGetParam( p_params, 1 )->intVal;
	Result const ret = SocketListen( this, backlog );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketAccept( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	SocketInfo * const out_connect = api->umkaGetParam( p_params, 1 )->ptrVal;
	Result const ret = SocketAccept( this, out_connect );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketConnect( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	cstr const address = api->umkaGetParam( p_params, 1 )->ptrVal;
	cstr const port = api->umkaGetParam( p_params, 2 )->ptrVal;
	SocketInfo * const out_connect = api->umkaGetParam( p_params, 3 )->ptrVal;
	Result const ret = SocketConnect( this, address, port, out_connect );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketGetReceiveSize( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	u64 * const out_size = api->umkaGetParam( p_params, 1 )->ptrVal;
	Result const ret = SocketGetReceiveSize( this->Socket, out_size );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketSend( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	UmkaDynArray( u8 ) * const buffer = api->umkaGetParam( p_params, 1 )->ptrVal;
	u64 * const out_send = api->umkaGetParam( p_params, 2 )->ptrVal;
	Result const ret = SocketSend( this->Socket, buffer->data, (u64)buffer->itemSize, out_send );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketSendText( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	cstr const buffer = api->umkaGetParam( p_params, 1 )->ptrVal;
	u64 * const out_send = api->umkaGetParam( p_params, 2 )->ptrVal;
	Result const ret = SocketSend( this->Socket, buffer, strlen( buffer ), out_send );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketReceive( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	UmkaDynArray( u8 ) * const buffer = api->umkaGetParam( p_params, 1 )->ptrVal;
	UmkaType const * const buffer_ptr_type = api->umkaGetParamType( p_params, 1 );
	UmkaType const * const buffer_type = api->umkaGetBaseType( buffer_ptr_type );
	u64 * const out_read = api->umkaGetParam( p_params, 2 )->ptrVal;

	u8 * buf = NULL;
	u64 bufs = 0;
	u64 read = 0;
	Result ret = SocketReceive( this->Socket, (ptr *)&buf, &bufs, &read );
	if ( ret.Code != 0 ) {
		UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
		*res = (UResult) {
			.Code = ret.Code,
			.Message = api->umkaMakeStr( umka, ret.Message )
		};
		return;
	}

	api->umkaMakeDynArray( umka, buffer, buffer_type, (int)read );
	memcpy( buffer->data, buf, read );
	free( buf );
	*out_read = read;

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}

UMKA_EXPORT void umc_SocketShutdown( UmkaStackSlot * p_params, UmkaStackSlot * p_result ) {
	Umka * umka = umkaGetInstance( p_result );
	UmkaAPI * api = umkaGetAPI( umka );

	SocketInfo * const this = api->umkaGetParam( p_params, 0 )->ptrVal;
	SocketChannel const channel = (SocketChannel)api->umkaGetParam( p_params, 1 )->intVal;
	Result const ret = SocketShutdown( this, channel );

	UResult * const res = api->umkaGetResult( p_params, p_result )->ptrVal;
	*res = (UResult) {
		.Code = ret.Code,
		.Message = api->umkaMakeStr( umka, ret.Message )
	};
}
