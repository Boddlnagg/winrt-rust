// DO NOT MODIFY THIS FILE - IT IS AUTOMATICALLY GENERATED!
#![allow(non_camel_case_types, unused_imports)]
pub mod windows { // Windows
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
pub mod devices { // Windows.Devices
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_STRUCT! { struct DevicesLowLevelContract {
			
		}}
		DEFINE_IID!(IID_ILowLevelDevicesAggregateProvider, 2805880348, 43713, 20167, 168, 82, 71, 159, 112, 96, 208, 31);
		RT_INTERFACE!{interface ILowLevelDevicesAggregateProvider(ILowLevelDevicesAggregateProviderVtbl): IInspectable(IInspectableVtbl) [IID_ILowLevelDevicesAggregateProvider] {
			fn get_AdcControllerProvider(&mut self, out: *mut *mut ::rt::gen::windows::devices::adc::provider::IAdcControllerProvider) -> ::w::HRESULT,
			fn get_PwmControllerProvider(&mut self, out: *mut *mut ::rt::gen::windows::devices::pwm::provider::IPwmControllerProvider) -> ::w::HRESULT,
			fn get_GpioControllerProvider(&mut self, out: *mut *mut ::rt::gen::windows::devices::gpio::provider::IGpioControllerProvider) -> ::w::HRESULT,
			fn get_I2cControllerProvider(&mut self, out: *mut *mut ::rt::gen::windows::devices::i2c::provider::II2cControllerProvider) -> ::w::HRESULT,
			fn get_SpiControllerProvider(&mut self, out: *mut *mut ::rt::gen::windows::devices::spi::provider::ISpiControllerProvider) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILowLevelDevicesAggregateProviderFactory, 2596580086, 13427, 18014, 150, 213, 54, 40, 26, 44, 87, 175);
		RT_INTERFACE!{interface ILowLevelDevicesAggregateProviderFactory(ILowLevelDevicesAggregateProviderFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ILowLevelDevicesAggregateProviderFactory] {
			fn Create(&mut self, adc: *mut ::rt::gen::windows::devices::adc::provider::IAdcControllerProvider, pwm: *mut ::rt::gen::windows::devices::pwm::provider::IPwmControllerProvider, gpio: *mut ::rt::gen::windows::devices::gpio::provider::IGpioControllerProvider, i2c: *mut ::rt::gen::windows::devices::i2c::provider::II2cControllerProvider, spi: *mut ::rt::gen::windows::devices::spi::provider::ISpiControllerProvider, out: *mut *mut ::rt::gen::windows::devices::LowLevelDevicesAggregateProvider) -> ::w::HRESULT
		}}
		RT_CLASS!(LowLevelDevicesAggregateProvider: ::rt::gen::windows::devices::ILowLevelDevicesAggregateProvider);
		DEFINE_IID!(IID_ILowLevelDevicesController, 784481748, 6043, 17886, 155, 57, 58, 224, 37, 39, 222, 82);
		RT_INTERFACE!{interface ILowLevelDevicesController(ILowLevelDevicesControllerVtbl): IInspectable(IInspectableVtbl) [IID_ILowLevelDevicesController] {
			
		}}
		DEFINE_IID!(IID_ILowLevelDevicesControllerStatics, 155095658, 64715, 17300, 166, 151, 25, 222, 99, 124, 45, 179);
		RT_INTERFACE!{interface ILowLevelDevicesControllerStatics(ILowLevelDevicesControllerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ILowLevelDevicesControllerStatics] {
			fn get_DefaultProvider(&mut self, out: *mut *mut ::rt::gen::windows::devices::ILowLevelDevicesAggregateProvider) -> ::w::HRESULT,
			fn put_DefaultProvider(&mut self, value: *mut ::rt::gen::windows::devices::ILowLevelDevicesAggregateProvider) -> ::w::HRESULT
		}}
		RT_CLASS!(LowLevelDevicesController: ::rt::gen::windows::devices::ILowLevelDevicesController);
pub mod adc { // Windows.Devices.Adc
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum AdcChannelMode: i32 {
			SingleEnded (AdcChannelMode_SingleEnded) = 0, Differential (AdcChannelMode_Differential) = 1,
		}}
		DEFINE_IID!(IID_IAdcController, 712434864, 43158, 16921, 134, 182, 234, 140, 220, 233, 143, 86);
		RT_INTERFACE!{interface IAdcController(IAdcControllerVtbl): IInspectable(IInspectableVtbl) [IID_IAdcController] {
			fn get_ChannelCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ResolutionInBits(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MinValue(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MaxValue(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ChannelMode(&mut self, out: *mut ::rt::gen::windows::devices::adc::AdcChannelMode) -> ::w::HRESULT,
			fn put_ChannelMode(&mut self, value: ::rt::gen::windows::devices::adc::AdcChannelMode) -> ::w::HRESULT,
			fn IsChannelModeSupported(&mut self, channelMode: ::rt::gen::windows::devices::adc::AdcChannelMode, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn OpenChannel(&mut self, channelNumber: i32, out: *mut *mut ::rt::gen::windows::devices::adc::AdcChannel) -> ::w::HRESULT
		}}
		RT_CLASS!(AdcChannel: ::rt::gen::windows::devices::adc::IAdcChannel);
		DEFINE_IID!(IID_IAdcControllerStatics, 3437858316, 504, 18577, 188, 59, 190, 83, 239, 39, 156, 164);
		RT_INTERFACE!{interface IAdcControllerStatics(IAdcControllerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IAdcControllerStatics] {
			fn GetControllersAsync(&mut self, provider: *mut ::rt::gen::windows::devices::adc::provider::IAdcProvider, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::adc::AdcController>>) -> ::w::HRESULT
		}}
		RT_CLASS!(AdcController: ::rt::gen::windows::devices::adc::IAdcController);
		DEFINE_IID!(IID_IAdcControllerStatics2, 2730048285, 38779, 20314, 165, 254, 166, 171, 175, 254, 100, 132);
		RT_INTERFACE!{interface IAdcControllerStatics2(IAdcControllerStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IAdcControllerStatics2] {
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::adc::AdcController>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAdcChannel, 67892244, 9608, 19030, 171, 239, 115, 162, 96, 172, 198, 10);
		RT_INTERFACE!{interface IAdcChannel(IAdcChannelVtbl): IInspectable(IInspectableVtbl) [IID_IAdcChannel] {
			fn get_Controller(&mut self, out: *mut *mut ::rt::gen::windows::devices::adc::AdcController) -> ::w::HRESULT,
			fn ReadValue(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn ReadRatio(&mut self, out: *mut f64) -> ::w::HRESULT
		}}
pub mod provider { // Windows.Devices.Adc.Provider
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum ProviderAdcChannelMode: i32 {
			SingleEnded (ProviderAdcChannelMode_SingleEnded) = 0, Differential (ProviderAdcChannelMode_Differential) = 1,
		}}
		DEFINE_IID!(IID_IAdcControllerProvider, 3193198632, 33133, 19941, 160, 72, 171, 160, 105, 88, 170, 168);
		RT_INTERFACE!{interface IAdcControllerProvider(IAdcControllerProviderVtbl): IInspectable(IInspectableVtbl) [IID_IAdcControllerProvider] {
			fn get_ChannelCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ResolutionInBits(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MinValue(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MaxValue(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ChannelMode(&mut self, out: *mut ::rt::gen::windows::devices::adc::provider::ProviderAdcChannelMode) -> ::w::HRESULT,
			fn put_ChannelMode(&mut self, value: ::rt::gen::windows::devices::adc::provider::ProviderAdcChannelMode) -> ::w::HRESULT,
			fn IsChannelModeSupported(&mut self, channelMode: ::rt::gen::windows::devices::adc::provider::ProviderAdcChannelMode, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn AcquireChannel(&mut self, channel: i32) -> ::w::HRESULT,
			fn ReleaseChannel(&mut self, channel: i32) -> ::w::HRESULT,
			fn ReadValue(&mut self, channelNumber: i32, out: *mut i32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAdcProvider, 680867432, 37721, 19543, 188, 136, 226, 117, 232, 22, 56, 201);
		RT_INTERFACE!{interface IAdcProvider(IAdcProviderVtbl): IInspectable(IInspectableVtbl) [IID_IAdcProvider] {
			fn GetControllers(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::adc::provider::IAdcControllerProvider>) -> ::w::HRESULT
		}}
} // Windows.Devices.Adc.Provider
} // Windows.Devices.Adc
pub mod gpio { // Windows.Devices.Gpio
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum GpioSharingMode: i32 {
			Exclusive (GpioSharingMode_Exclusive) = 0, SharedReadOnly (GpioSharingMode_SharedReadOnly) = 1,
		}}
		RT_ENUM! { enum GpioOpenStatus: i32 {
			PinOpened (GpioOpenStatus_PinOpened) = 0, PinUnavailable (GpioOpenStatus_PinUnavailable) = 1, SharingViolation (GpioOpenStatus_SharingViolation) = 2,
		}}
		RT_ENUM! { enum GpioPinDriveMode: i32 {
			Input (GpioPinDriveMode_Input) = 0, Output (GpioPinDriveMode_Output) = 1, InputPullUp (GpioPinDriveMode_InputPullUp) = 2, InputPullDown (GpioPinDriveMode_InputPullDown) = 3, OutputOpenDrain (GpioPinDriveMode_OutputOpenDrain) = 4, OutputOpenDrainPullUp (GpioPinDriveMode_OutputOpenDrainPullUp) = 5, OutputOpenSource (GpioPinDriveMode_OutputOpenSource) = 6, OutputOpenSourcePullDown (GpioPinDriveMode_OutputOpenSourcePullDown) = 7,
		}}
		RT_ENUM! { enum GpioPinValue: i32 {
			Low (GpioPinValue_Low) = 0, High (GpioPinValue_High) = 1,
		}}
		RT_ENUM! { enum GpioPinEdge: i32 {
			FallingEdge (GpioPinEdge_FallingEdge) = 0, RisingEdge (GpioPinEdge_RisingEdge) = 1,
		}}
		DEFINE_IID!(IID_IGpioPinValueChangedEventArgs, 825731809, 28733, 16473, 189, 36, 181, 178, 93, 255, 184, 78);
		RT_INTERFACE!{interface IGpioPinValueChangedEventArgs(IGpioPinValueChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IGpioPinValueChangedEventArgs] {
			fn get_Edge(&mut self, out: *mut ::rt::gen::windows::devices::gpio::GpioPinEdge) -> ::w::HRESULT
		}}
		RT_CLASS!(GpioPinValueChangedEventArgs: ::rt::gen::windows::devices::gpio::IGpioPinValueChangedEventArgs);
		RT_CLASS!(GpioPin: ::rt::gen::windows::devices::gpio::IGpioPin);
		DEFINE_IID!(IID_IGpioController, 675287779, 29793, 18076, 168, 188, 97, 214, 157, 8, 165, 60);
		RT_INTERFACE!{interface IGpioController(IGpioControllerVtbl): IInspectable(IInspectableVtbl) [IID_IGpioController] {
			fn get_PinCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn OpenPin(&mut self, pinNumber: i32, out: *mut *mut ::rt::gen::windows::devices::gpio::GpioPin) -> ::w::HRESULT,
			fn OpenPinWithSharingMode(&mut self, pinNumber: i32, sharingMode: ::rt::gen::windows::devices::gpio::GpioSharingMode, out: *mut *mut ::rt::gen::windows::devices::gpio::GpioPin) -> ::w::HRESULT,
			fn TryOpenPin(&mut self, pinNumber: i32, sharingMode: ::rt::gen::windows::devices::gpio::GpioSharingMode, pin: *mut *mut ::rt::gen::windows::devices::gpio::GpioPin, openStatus: *mut ::rt::gen::windows::devices::gpio::GpioOpenStatus, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGpioControllerStatics, 785839150, 31479, 16662, 149, 51, 196, 61, 153, 161, 251, 100);
		RT_INTERFACE!{interface IGpioControllerStatics(IGpioControllerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGpioControllerStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::gpio::GpioController) -> ::w::HRESULT
		}}
		RT_CLASS!(GpioController: ::rt::gen::windows::devices::gpio::IGpioController);
		DEFINE_IID!(IID_IGpioControllerStatics2, 2435546400, 27812, 16646, 163, 115, 255, 253, 52, 107, 14, 91);
		RT_INTERFACE!{interface IGpioControllerStatics2(IGpioControllerStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IGpioControllerStatics2] {
			fn GetControllersAsync(&mut self, provider: *mut ::rt::gen::windows::devices::gpio::provider::IGpioProvider, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::gpio::GpioController>>) -> ::w::HRESULT,
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::gpio::GpioController>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGpioPin, 299479175, 44974, 18320, 158, 233, 224, 234, 201, 66, 210, 1);
		RT_INTERFACE!{interface IGpioPin(IGpioPinVtbl): IInspectable(IInspectableVtbl) [IID_IGpioPin] {
			fn add_ValueChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::gpio::GpioPin, &::rt::gen::windows::devices::gpio::GpioPinValueChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ValueChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_DebounceTimeout(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn put_DebounceTimeout(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_PinNumber(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_SharingMode(&mut self, out: *mut ::rt::gen::windows::devices::gpio::GpioSharingMode) -> ::w::HRESULT,
			fn IsDriveModeSupported(&mut self, driveMode: ::rt::gen::windows::devices::gpio::GpioPinDriveMode, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn GetDriveMode(&mut self, out: *mut ::rt::gen::windows::devices::gpio::GpioPinDriveMode) -> ::w::HRESULT,
			fn SetDriveMode(&mut self, value: ::rt::gen::windows::devices::gpio::GpioPinDriveMode) -> ::w::HRESULT,
			fn Write(&mut self, value: ::rt::gen::windows::devices::gpio::GpioPinValue) -> ::w::HRESULT,
			fn Read(&mut self, out: *mut ::rt::gen::windows::devices::gpio::GpioPinValue) -> ::w::HRESULT
		}}
pub mod provider { // Windows.Devices.Gpio.Provider
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum ProviderGpioSharingMode: i32 {
			Exclusive (ProviderGpioSharingMode_Exclusive) = 0, SharedReadOnly (ProviderGpioSharingMode_SharedReadOnly) = 1,
		}}
		RT_ENUM! { enum ProviderGpioPinDriveMode: i32 {
			Input (ProviderGpioPinDriveMode_Input) = 0, Output (ProviderGpioPinDriveMode_Output) = 1, InputPullUp (ProviderGpioPinDriveMode_InputPullUp) = 2, InputPullDown (ProviderGpioPinDriveMode_InputPullDown) = 3, OutputOpenDrain (ProviderGpioPinDriveMode_OutputOpenDrain) = 4, OutputOpenDrainPullUp (ProviderGpioPinDriveMode_OutputOpenDrainPullUp) = 5, OutputOpenSource (ProviderGpioPinDriveMode_OutputOpenSource) = 6, OutputOpenSourcePullDown (ProviderGpioPinDriveMode_OutputOpenSourcePullDown) = 7,
		}}
		RT_ENUM! { enum ProviderGpioPinValue: i32 {
			Low (ProviderGpioPinValue_Low) = 0, High (ProviderGpioPinValue_High) = 1,
		}}
		RT_ENUM! { enum ProviderGpioPinEdge: i32 {
			FallingEdge (ProviderGpioPinEdge_FallingEdge) = 0, RisingEdge (ProviderGpioPinEdge_RisingEdge) = 1,
		}}
		DEFINE_IID!(IID_IGpioPinProviderValueChangedEventArgsFactory, 1053494105, 22156, 17298, 178, 74, 138, 89, 169, 2, 177, 241);
		RT_INTERFACE!{interface IGpioPinProviderValueChangedEventArgsFactory(IGpioPinProviderValueChangedEventArgsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IGpioPinProviderValueChangedEventArgsFactory] {
			fn Create(&mut self, edge: ::rt::gen::windows::devices::gpio::provider::ProviderGpioPinEdge, out: *mut *mut ::rt::gen::windows::devices::gpio::provider::GpioPinProviderValueChangedEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(GpioPinProviderValueChangedEventArgs: ::rt::gen::windows::devices::gpio::provider::IGpioPinProviderValueChangedEventArgs);
		DEFINE_IID!(IID_IGpioPinProviderValueChangedEventArgs, 849794802, 15707, 17613, 143, 190, 19, 166, 159, 46, 219, 36);
		RT_INTERFACE!{interface IGpioPinProviderValueChangedEventArgs(IGpioPinProviderValueChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IGpioPinProviderValueChangedEventArgs] {
			fn get_Edge(&mut self, out: *mut ::rt::gen::windows::devices::gpio::provider::ProviderGpioPinEdge) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGpioPinProvider, 1110723767, 27324, 16639, 156, 231, 115, 184, 83, 1, 185, 0);
		RT_INTERFACE!{interface IGpioPinProvider(IGpioPinProviderVtbl): IInspectable(IInspectableVtbl) [IID_IGpioPinProvider] {
			fn add_ValueChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::gpio::provider::IGpioPinProvider, &::rt::gen::windows::devices::gpio::provider::GpioPinProviderValueChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ValueChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_DebounceTimeout(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn put_DebounceTimeout(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_PinNumber(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_SharingMode(&mut self, out: *mut ::rt::gen::windows::devices::gpio::provider::ProviderGpioSharingMode) -> ::w::HRESULT,
			fn IsDriveModeSupported(&mut self, driveMode: ::rt::gen::windows::devices::gpio::provider::ProviderGpioPinDriveMode, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn GetDriveMode(&mut self, out: *mut ::rt::gen::windows::devices::gpio::provider::ProviderGpioPinDriveMode) -> ::w::HRESULT,
			fn SetDriveMode(&mut self, value: ::rt::gen::windows::devices::gpio::provider::ProviderGpioPinDriveMode) -> ::w::HRESULT,
			fn Write(&mut self, value: ::rt::gen::windows::devices::gpio::provider::ProviderGpioPinValue) -> ::w::HRESULT,
			fn Read(&mut self, out: *mut ::rt::gen::windows::devices::gpio::provider::ProviderGpioPinValue) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGpioControllerProvider, 2903625415, 6634, 19233, 135, 79, 185, 26, 237, 74, 37, 219);
		RT_INTERFACE!{interface IGpioControllerProvider(IGpioControllerProviderVtbl): IInspectable(IInspectableVtbl) [IID_IGpioControllerProvider] {
			fn get_PinCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn OpenPinProvider(&mut self, pin: i32, sharingMode: ::rt::gen::windows::devices::gpio::provider::ProviderGpioSharingMode, out: *mut *mut ::rt::gen::windows::devices::gpio::provider::IGpioPinProvider) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGpioProvider, 1156065031, 2250, 17226, 175, 224, 214, 21, 128, 68, 111, 126);
		RT_INTERFACE!{interface IGpioProvider(IGpioProviderVtbl): IInspectable(IInspectableVtbl) [IID_IGpioProvider] {
			fn GetControllers(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::gpio::provider::IGpioControllerProvider>) -> ::w::HRESULT
		}}
} // Windows.Devices.Gpio.Provider
} // Windows.Devices.Gpio
pub mod i2c { // Windows.Devices.I2c
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum I2cBusSpeed: i32 {
			StandardMode (I2cBusSpeed_StandardMode) = 0, FastMode (I2cBusSpeed_FastMode) = 1,
		}}
		RT_ENUM! { enum I2cTransferStatus: i32 {
			FullTransfer (I2cTransferStatus_FullTransfer) = 0, PartialTransfer (I2cTransferStatus_PartialTransfer) = 1, SlaveAddressNotAcknowledged (I2cTransferStatus_SlaveAddressNotAcknowledged) = 2,
		}}
		RT_ENUM! { enum I2cSharingMode: i32 {
			Exclusive (I2cSharingMode_Exclusive) = 0, Shared (I2cSharingMode_Shared) = 1,
		}}
		RT_STRUCT! { struct I2cTransferResult {
			Status: ::rt::gen::windows::devices::i2c::I2cTransferStatus, BytesTransferred: u32,
		}}
		DEFINE_IID!(IID_II2cConnectionSettingsFactory, 2176157363, 38547, 16817, 162, 67, 222, 212, 246, 230, 105, 38);
		RT_INTERFACE!{interface II2cConnectionSettingsFactory(II2cConnectionSettingsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_II2cConnectionSettingsFactory] {
			fn Create(&mut self, slaveAddress: i32, out: *mut *mut ::rt::gen::windows::devices::i2c::I2cConnectionSettings) -> ::w::HRESULT
		}}
		RT_CLASS!(I2cConnectionSettings: ::rt::gen::windows::devices::i2c::II2cConnectionSettings);
		DEFINE_IID!(IID_II2cConnectionSettings, 4074443527, 43887, 17977, 167, 103, 84, 83, 109, 195, 70, 15);
		RT_INTERFACE!{interface II2cConnectionSettings(II2cConnectionSettingsVtbl): IInspectable(IInspectableVtbl) [IID_II2cConnectionSettings] {
			fn get_SlaveAddress(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_SlaveAddress(&mut self, value: i32) -> ::w::HRESULT,
			fn get_BusSpeed(&mut self, out: *mut ::rt::gen::windows::devices::i2c::I2cBusSpeed) -> ::w::HRESULT,
			fn put_BusSpeed(&mut self, value: ::rt::gen::windows::devices::i2c::I2cBusSpeed) -> ::w::HRESULT,
			fn get_SharingMode(&mut self, out: *mut ::rt::gen::windows::devices::i2c::I2cSharingMode) -> ::w::HRESULT,
			fn put_SharingMode(&mut self, value: ::rt::gen::windows::devices::i2c::I2cSharingMode) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_II2cDeviceStatics, 2443394019, 29492, 17682, 150, 188, 251, 174, 148, 89, 245, 246);
		RT_INTERFACE!{interface II2cDeviceStatics(II2cDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_II2cDeviceStatics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromFriendlyName(&mut self, friendlyName: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, settings: *mut ::rt::gen::windows::devices::i2c::I2cConnectionSettings, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::i2c::I2cDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(I2cDevice: ::rt::gen::windows::devices::i2c::II2cDevice);
		DEFINE_IID!(IID_II2cController, 3297423794, 34720, 16742, 142, 62, 180, 184, 249, 124, 215, 41);
		RT_INTERFACE!{interface II2cController(II2cControllerVtbl): IInspectable(IInspectableVtbl) [IID_II2cController] {
			fn GetDevice(&mut self, settings: *mut ::rt::gen::windows::devices::i2c::I2cConnectionSettings, out: *mut *mut ::rt::gen::windows::devices::i2c::I2cDevice) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_II2cControllerStatics, 1090257765, 24325, 20094, 132, 189, 16, 13, 184, 224, 174, 197);
		RT_INTERFACE!{interface II2cControllerStatics(II2cControllerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_II2cControllerStatics] {
			fn GetControllersAsync(&mut self, provider: *mut ::rt::gen::windows::devices::i2c::provider::II2cProvider, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::i2c::I2cController>>) -> ::w::HRESULT,
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::i2c::I2cController>) -> ::w::HRESULT
		}}
		RT_CLASS!(I2cController: ::rt::gen::windows::devices::i2c::II2cController);
		DEFINE_IID!(IID_II2cDevice, 2251735350, 47557, 20336, 148, 73, 204, 70, 220, 111, 87, 235);
		RT_INTERFACE!{interface II2cDevice(II2cDeviceVtbl): IInspectable(IInspectableVtbl) [IID_II2cDevice] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ConnectionSettings(&mut self, out: *mut *mut ::rt::gen::windows::devices::i2c::I2cConnectionSettings) -> ::w::HRESULT,
			fn Write(&mut self, buffer: *mut u8) -> ::w::HRESULT,
			fn WritePartial(&mut self, buffer: *mut u8, out: *mut ::rt::gen::windows::devices::i2c::I2cTransferResult) -> ::w::HRESULT,
			fn Read(&mut self, buffer: *mut u8) -> ::w::HRESULT,
			fn ReadPartial(&mut self, buffer: *mut u8, out: *mut ::rt::gen::windows::devices::i2c::I2cTransferResult) -> ::w::HRESULT,
			fn WriteRead(&mut self, writeBuffer: *mut u8, readBuffer: *mut u8) -> ::w::HRESULT,
			fn WriteReadPartial(&mut self, writeBuffer: *mut u8, readBuffer: *mut u8, out: *mut ::rt::gen::windows::devices::i2c::I2cTransferResult) -> ::w::HRESULT
		}}
pub mod provider { // Windows.Devices.I2c.Provider
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum ProviderI2cBusSpeed: i32 {
			StandardMode (ProviderI2cBusSpeed_StandardMode) = 0, FastMode (ProviderI2cBusSpeed_FastMode) = 1,
		}}
		RT_ENUM! { enum ProviderI2cTransferStatus: i32 {
			FullTransfer (ProviderI2cTransferStatus_FullTransfer) = 0, PartialTransfer (ProviderI2cTransferStatus_PartialTransfer) = 1, SlaveAddressNotAcknowledged (ProviderI2cTransferStatus_SlaveAddressNotAcknowledged) = 2,
		}}
		RT_ENUM! { enum ProviderI2cSharingMode: i32 {
			Exclusive (ProviderI2cSharingMode_Exclusive) = 0, Shared (ProviderI2cSharingMode_Shared) = 1,
		}}
		RT_STRUCT! { struct ProviderI2cTransferResult {
			Status: ::rt::gen::windows::devices::i2c::provider::ProviderI2cTransferStatus, BytesTransferred: u32,
		}}
		DEFINE_IID!(IID_IProviderI2cConnectionSettings, 3923463732, 58640, 17591, 128, 157, 242, 248, 91, 85, 83, 57);
		RT_INTERFACE!{interface IProviderI2cConnectionSettings(IProviderI2cConnectionSettingsVtbl): IInspectable(IInspectableVtbl) [IID_IProviderI2cConnectionSettings] {
			fn get_SlaveAddress(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_SlaveAddress(&mut self, value: i32) -> ::w::HRESULT,
			fn get_BusSpeed(&mut self, out: *mut ::rt::gen::windows::devices::i2c::provider::ProviderI2cBusSpeed) -> ::w::HRESULT,
			fn put_BusSpeed(&mut self, value: ::rt::gen::windows::devices::i2c::provider::ProviderI2cBusSpeed) -> ::w::HRESULT,
			fn get_SharingMode(&mut self, out: *mut ::rt::gen::windows::devices::i2c::provider::ProviderI2cSharingMode) -> ::w::HRESULT,
			fn put_SharingMode(&mut self, value: ::rt::gen::windows::devices::i2c::provider::ProviderI2cSharingMode) -> ::w::HRESULT
		}}
		RT_CLASS!(ProviderI2cConnectionSettings: ::rt::gen::windows::devices::i2c::provider::IProviderI2cConnectionSettings);
		DEFINE_IID!(IID_II2cControllerProvider, 1640151938, 17680, 16739, 168, 124, 78, 21, 169, 85, 137, 128);
		RT_INTERFACE!{interface II2cControllerProvider(II2cControllerProviderVtbl): IInspectable(IInspectableVtbl) [IID_II2cControllerProvider] {
			fn GetDeviceProvider(&mut self, settings: *mut ::rt::gen::windows::devices::i2c::provider::ProviderI2cConnectionSettings, out: *mut *mut ::rt::gen::windows::devices::i2c::provider::II2cDeviceProvider) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_II2cProvider, 1863518270, 48994, 20450, 169, 90, 240, 137, 153, 102, 152, 24);
		RT_INTERFACE!{interface II2cProvider(II2cProviderVtbl): IInspectable(IInspectableVtbl) [IID_II2cProvider] {
			fn GetControllersAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::i2c::provider::II2cControllerProvider>>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_II2cDeviceProvider, 2905876052, 22504, 17726, 131, 41, 209, 228, 71, 209, 3, 169);
		RT_INTERFACE!{interface II2cDeviceProvider(II2cDeviceProviderVtbl): IInspectable(IInspectableVtbl) [IID_II2cDeviceProvider] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn Write(&mut self, buffer: *mut u8) -> ::w::HRESULT,
			fn WritePartial(&mut self, buffer: *mut u8, out: *mut ::rt::gen::windows::devices::i2c::provider::ProviderI2cTransferResult) -> ::w::HRESULT,
			fn Read(&mut self, buffer: *mut u8) -> ::w::HRESULT,
			fn ReadPartial(&mut self, buffer: *mut u8, out: *mut ::rt::gen::windows::devices::i2c::provider::ProviderI2cTransferResult) -> ::w::HRESULT,
			fn WriteRead(&mut self, writeBuffer: *mut u8, readBuffer: *mut u8) -> ::w::HRESULT,
			fn WriteReadPartial(&mut self, writeBuffer: *mut u8, readBuffer: *mut u8, out: *mut ::rt::gen::windows::devices::i2c::provider::ProviderI2cTransferResult) -> ::w::HRESULT
		}}
} // Windows.Devices.I2c.Provider
} // Windows.Devices.I2c
pub mod pwm { // Windows.Devices.Pwm
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum PwmPulsePolarity: i32 {
			ActiveHigh (PwmPulsePolarity_ActiveHigh) = 0, ActiveLow (PwmPulsePolarity_ActiveLow) = 1,
		}}
		DEFINE_IID!(IID_IPwmController, 3294583941, 53992, 17103, 155, 214, 207, 94, 208, 41, 230, 167);
		RT_INTERFACE!{interface IPwmController(IPwmControllerVtbl): IInspectable(IInspectableVtbl) [IID_IPwmController] {
			fn get_PinCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ActualFrequency(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn SetDesiredFrequency(&mut self, desiredFrequency: f64, out: *mut f64) -> ::w::HRESULT,
			fn get_MinFrequency(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_MaxFrequency(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn OpenPin(&mut self, pinNumber: i32, out: *mut *mut ::rt::gen::windows::devices::pwm::PwmPin) -> ::w::HRESULT
		}}
		RT_CLASS!(PwmPin: ::rt::gen::windows::devices::pwm::IPwmPin);
		DEFINE_IID!(IID_IPwmControllerStatics, 1113832865, 35142, 17412, 189, 72, 129, 221, 18, 74, 244, 217);
		RT_INTERFACE!{interface IPwmControllerStatics(IPwmControllerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPwmControllerStatics] {
			fn GetControllersAsync(&mut self, provider: *mut ::rt::gen::windows::devices::pwm::provider::IPwmProvider, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::pwm::PwmController>>) -> ::w::HRESULT
		}}
		RT_CLASS!(PwmController: ::rt::gen::windows::devices::pwm::IPwmController);
		DEFINE_IID!(IID_IPwmControllerStatics2, 1157389087, 61721, 19421, 151, 173, 247, 110, 249, 134, 115, 109);
		RT_INTERFACE!{interface IPwmControllerStatics2(IPwmControllerStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IPwmControllerStatics2] {
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pwm::PwmController>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPwmPin, 580333000, 50895, 18465, 183, 249, 198, 69, 79, 182, 175, 121);
		RT_INTERFACE!{interface IPwmPin(IPwmPinVtbl): IInspectable(IInspectableVtbl) [IID_IPwmPin] {
			fn get_Controller(&mut self, out: *mut *mut ::rt::gen::windows::devices::pwm::PwmController) -> ::w::HRESULT,
			fn GetActiveDutyCyclePercentage(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn SetActiveDutyCyclePercentage(&mut self, dutyCyclePercentage: f64) -> ::w::HRESULT,
			fn get_Polarity(&mut self, out: *mut ::rt::gen::windows::devices::pwm::PwmPulsePolarity) -> ::w::HRESULT,
			fn put_Polarity(&mut self, value: ::rt::gen::windows::devices::pwm::PwmPulsePolarity) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT,
			fn get_IsStarted(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
pub mod provider { // Windows.Devices.Pwm.Provider
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_IPwmControllerProvider, 318789947, 58083, 16548, 183, 217, 72, 223, 240, 55, 122, 82);
		RT_INTERFACE!{interface IPwmControllerProvider(IPwmControllerProviderVtbl): IInspectable(IInspectableVtbl) [IID_IPwmControllerProvider] {
			fn get_PinCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ActualFrequency(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn SetDesiredFrequency(&mut self, frequency: f64, out: *mut f64) -> ::w::HRESULT,
			fn get_MaxFrequency(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_MinFrequency(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn AcquirePin(&mut self, pin: i32) -> ::w::HRESULT,
			fn ReleasePin(&mut self, pin: i32) -> ::w::HRESULT,
			fn EnablePin(&mut self, pin: i32) -> ::w::HRESULT,
			fn DisablePin(&mut self, pin: i32) -> ::w::HRESULT,
			fn SetPulseParameters(&mut self, pin: i32, dutyCycle: f64, invertPolarity: ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPwmProvider, 2737836584, 21233, 18352, 147, 73, 102, 186, 67, 210, 89, 2);
		RT_INTERFACE!{interface IPwmProvider(IPwmProviderVtbl): IInspectable(IInspectableVtbl) [IID_IPwmProvider] {
			fn GetControllers(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::pwm::provider::IPwmControllerProvider>) -> ::w::HRESULT
		}}
} // Windows.Devices.Pwm.Provider
} // Windows.Devices.Pwm
pub mod spi { // Windows.Devices.Spi
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum SpiMode: i32 {
			Mode0 (SpiMode_Mode0) = 0, Mode1 (SpiMode_Mode1) = 1, Mode2 (SpiMode_Mode2) = 2, Mode3 (SpiMode_Mode3) = 3,
		}}
		RT_ENUM! { enum SpiSharingMode: i32 {
			Exclusive (SpiSharingMode_Exclusive) = 0, Shared (SpiSharingMode_Shared) = 1,
		}}
		DEFINE_IID!(IID_ISpiConnectionSettingsFactory, 4288219166, 4292, 17591, 159, 234, 167, 72, 181, 164, 111, 49);
		RT_INTERFACE!{interface ISpiConnectionSettingsFactory(ISpiConnectionSettingsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ISpiConnectionSettingsFactory] {
			fn Create(&mut self, chipSelectLine: i32, out: *mut *mut ::rt::gen::windows::devices::spi::SpiConnectionSettings) -> ::w::HRESULT
		}}
		RT_CLASS!(SpiConnectionSettings: ::rt::gen::windows::devices::spi::ISpiConnectionSettings);
		DEFINE_IID!(IID_ISpiConnectionSettings, 1384358783, 63797, 19359, 167, 167, 58, 120, 144, 175, 165, 206);
		RT_INTERFACE!{interface ISpiConnectionSettings(ISpiConnectionSettingsVtbl): IInspectable(IInspectableVtbl) [IID_ISpiConnectionSettings] {
			fn get_ChipSelectLine(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_ChipSelectLine(&mut self, value: i32) -> ::w::HRESULT,
			fn get_Mode(&mut self, out: *mut ::rt::gen::windows::devices::spi::SpiMode) -> ::w::HRESULT,
			fn put_Mode(&mut self, value: ::rt::gen::windows::devices::spi::SpiMode) -> ::w::HRESULT,
			fn get_DataBitLength(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_DataBitLength(&mut self, value: i32) -> ::w::HRESULT,
			fn get_ClockFrequency(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_ClockFrequency(&mut self, value: i32) -> ::w::HRESULT,
			fn get_SharingMode(&mut self, out: *mut ::rt::gen::windows::devices::spi::SpiSharingMode) -> ::w::HRESULT,
			fn put_SharingMode(&mut self, value: ::rt::gen::windows::devices::spi::SpiSharingMode) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISpiBusInfo, 2569618506, 21746, 18630, 185, 82, 156, 50, 252, 2, 198, 105);
		RT_INTERFACE!{interface ISpiBusInfo(ISpiBusInfoVtbl): IInspectable(IInspectableVtbl) [IID_ISpiBusInfo] {
			fn get_ChipSelectLineCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MinClockFrequency(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MaxClockFrequency(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_SupportedDataBitLengths(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<i32>) -> ::w::HRESULT
		}}
		RT_CLASS!(SpiBusInfo: ::rt::gen::windows::devices::spi::ISpiBusInfo);
		DEFINE_IID!(IID_ISpiDeviceStatics, 2725832025, 22304, 19775, 189, 147, 86, 245, 255, 90, 88, 121);
		RT_INTERFACE!{interface ISpiDeviceStatics(ISpiDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISpiDeviceStatics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromFriendlyName(&mut self, friendlyName: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetBusInfo(&mut self, busId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::spi::SpiBusInfo) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, busId: ::w::HSTRING, settings: *mut ::rt::gen::windows::devices::spi::SpiConnectionSettings, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::spi::SpiDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(SpiDevice: ::rt::gen::windows::devices::spi::ISpiDevice);
		DEFINE_IID!(IID_ISpiController, 2832451625, 39061, 16729, 169, 52, 135, 65, 241, 238, 109, 39);
		RT_INTERFACE!{interface ISpiController(ISpiControllerVtbl): IInspectable(IInspectableVtbl) [IID_ISpiController] {
			fn GetDevice(&mut self, settings: *mut ::rt::gen::windows::devices::spi::SpiConnectionSettings, out: *mut *mut ::rt::gen::windows::devices::spi::SpiDevice) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISpiControllerStatics, 223488482, 5003, 20040, 185, 100, 79, 47, 121, 185, 197, 162);
		RT_INTERFACE!{interface ISpiControllerStatics(ISpiControllerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISpiControllerStatics] {
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::spi::SpiController>) -> ::w::HRESULT,
			fn GetControllersAsync(&mut self, provider: *mut ::rt::gen::windows::devices::spi::provider::ISpiProvider, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::spi::SpiController>>) -> ::w::HRESULT
		}}
		RT_CLASS!(SpiController: ::rt::gen::windows::devices::spi::ISpiController);
		DEFINE_IID!(IID_ISpiDevice, 97858925, 4534, 19769, 132, 213, 149, 223, 180, 201, 242, 206);
		RT_INTERFACE!{interface ISpiDevice(ISpiDeviceVtbl): IInspectable(IInspectableVtbl) [IID_ISpiDevice] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ConnectionSettings(&mut self, out: *mut *mut ::rt::gen::windows::devices::spi::SpiConnectionSettings) -> ::w::HRESULT,
			fn Write(&mut self, buffer: *mut u8) -> ::w::HRESULT,
			fn Read(&mut self, buffer: *mut u8) -> ::w::HRESULT,
			fn TransferSequential(&mut self, writeBuffer: *mut u8, readBuffer: *mut u8) -> ::w::HRESULT,
			fn TransferFullDuplex(&mut self, writeBuffer: *mut u8, readBuffer: *mut u8) -> ::w::HRESULT
		}}
pub mod provider { // Windows.Devices.Spi.Provider
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum ProviderSpiMode: i32 {
			Mode0 (ProviderSpiMode_Mode0) = 0, Mode1 (ProviderSpiMode_Mode1) = 1, Mode2 (ProviderSpiMode_Mode2) = 2, Mode3 (ProviderSpiMode_Mode3) = 3,
		}}
		RT_ENUM! { enum ProviderSpiSharingMode: i32 {
			Exclusive (ProviderSpiSharingMode_Exclusive) = 0, Shared (ProviderSpiSharingMode_Shared) = 1,
		}}
		DEFINE_IID!(IID_IProviderSpiConnectionSettingsFactory, 1715825498, 3193, 17379, 159, 60, 229, 151, 128, 172, 24, 250);
		RT_INTERFACE!{interface IProviderSpiConnectionSettingsFactory(IProviderSpiConnectionSettingsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IProviderSpiConnectionSettingsFactory] {
			fn Create(&mut self, chipSelectLine: i32, out: *mut *mut ::rt::gen::windows::devices::spi::provider::ProviderSpiConnectionSettings) -> ::w::HRESULT
		}}
		RT_CLASS!(ProviderSpiConnectionSettings: ::rt::gen::windows::devices::spi::provider::IProviderSpiConnectionSettings);
		DEFINE_IID!(IID_IProviderSpiConnectionSettings, 4127409488, 42306, 20160, 150, 1, 164, 221, 104, 248, 105, 123);
		RT_INTERFACE!{interface IProviderSpiConnectionSettings(IProviderSpiConnectionSettingsVtbl): IInspectable(IInspectableVtbl) [IID_IProviderSpiConnectionSettings] {
			fn get_ChipSelectLine(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_ChipSelectLine(&mut self, value: i32) -> ::w::HRESULT,
			fn get_Mode(&mut self, out: *mut ::rt::gen::windows::devices::spi::provider::ProviderSpiMode) -> ::w::HRESULT,
			fn put_Mode(&mut self, value: ::rt::gen::windows::devices::spi::provider::ProviderSpiMode) -> ::w::HRESULT,
			fn get_DataBitLength(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_DataBitLength(&mut self, value: i32) -> ::w::HRESULT,
			fn get_ClockFrequency(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_ClockFrequency(&mut self, value: i32) -> ::w::HRESULT,
			fn get_SharingMode(&mut self, out: *mut ::rt::gen::windows::devices::spi::provider::ProviderSpiSharingMode) -> ::w::HRESULT,
			fn put_SharingMode(&mut self, value: ::rt::gen::windows::devices::spi::provider::ProviderSpiSharingMode) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISpiControllerProvider, 3244844292, 718, 16934, 163, 133, 79, 17, 251, 4, 180, 27);
		RT_INTERFACE!{interface ISpiControllerProvider(ISpiControllerProviderVtbl): IInspectable(IInspectableVtbl) [IID_ISpiControllerProvider] {
			fn GetDeviceProvider(&mut self, settings: *mut ::rt::gen::windows::devices::spi::provider::ProviderSpiConnectionSettings, out: *mut *mut ::rt::gen::windows::devices::spi::provider::ISpiDeviceProvider) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISpiProvider, 2528403938, 30676, 18638, 170, 160, 117, 113, 90, 131, 98, 207);
		RT_INTERFACE!{interface ISpiProvider(ISpiProviderVtbl): IInspectable(IInspectableVtbl) [IID_ISpiProvider] {
			fn GetControllersAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::spi::provider::ISpiControllerProvider>>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISpiDeviceProvider, 219952195, 12363, 16476, 180, 247, 245, 171, 16, 116, 70, 30);
		RT_INTERFACE!{interface ISpiDeviceProvider(ISpiDeviceProviderVtbl): IInspectable(IInspectableVtbl) [IID_ISpiDeviceProvider] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ConnectionSettings(&mut self, out: *mut *mut ::rt::gen::windows::devices::spi::provider::ProviderSpiConnectionSettings) -> ::w::HRESULT,
			fn Write(&mut self, buffer: *mut u8) -> ::w::HRESULT,
			fn Read(&mut self, buffer: *mut u8) -> ::w::HRESULT,
			fn TransferSequential(&mut self, writeBuffer: *mut u8, readBuffer: *mut u8) -> ::w::HRESULT,
			fn TransferFullDuplex(&mut self, writeBuffer: *mut u8, readBuffer: *mut u8) -> ::w::HRESULT
		}}
} // Windows.Devices.Spi.Provider
} // Windows.Devices.Spi
pub mod printers { // Windows.Devices.Printers
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_STRUCT! { struct PrintersContract {
			
		}}
		DEFINE_IID!(IID_IPrint3DDeviceStatics, 4259537418, 26573, 16823, 163, 68, 81, 80, 161, 253, 117, 181);
		RT_INTERFACE!{interface IPrint3DDeviceStatics(IPrint3DDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPrint3DDeviceStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::printers::Print3DDevice>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(Print3DDevice: ::rt::gen::windows::devices::printers::IPrint3DDevice);
		DEFINE_IID!(IID_IPrint3DDevice, 68959513, 38675, 17058, 152, 19, 125, 195, 51, 116, 40, 211);
		RT_INTERFACE!{interface IPrint3DDevice(IPrint3DDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IPrint3DDevice] {
			fn get_PrintSchema(&mut self, out: *mut *mut ::rt::gen::windows::devices::printers::PrintSchema) -> ::w::HRESULT
		}}
		RT_CLASS!(PrintSchema: ::rt::gen::windows::devices::printers::IPrintSchema);
		DEFINE_IID!(IID_IPrintSchema, 3266937622, 9912, 19451, 129, 56, 159, 150, 44, 34, 163, 91);
		RT_INTERFACE!{interface IPrintSchema(IPrintSchemaVtbl): IInspectable(IInspectableVtbl) [IID_IPrintSchema] {
			fn GetDefaultPrintTicketAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType>) -> ::w::HRESULT,
			fn GetCapabilitiesAsync(&mut self, constrainTicket: *mut ::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType>) -> ::w::HRESULT,
			fn MergeAndValidateWithDefaultPrintTicketAsync(&mut self, deltaTicket: *mut ::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType>) -> ::w::HRESULT
		}}
pub mod extensions { // Windows.Devices.Printers.Extensions
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum Print3DWorkflowStatus: i32 {
			Abandoned (Print3DWorkflowStatus_Abandoned) = 0, Canceled (Print3DWorkflowStatus_Canceled) = 1, Failed (Print3DWorkflowStatus_Failed) = 2, Slicing (Print3DWorkflowStatus_Slicing) = 3, Submitted (Print3DWorkflowStatus_Submitted) = 4,
		}}
		RT_ENUM! { enum Print3DWorkflowDetail: i32 {
			Unknown (Print3DWorkflowDetail_Unknown) = 0, ModelExceedsPrintBed (Print3DWorkflowDetail_ModelExceedsPrintBed) = 1, UploadFailed (Print3DWorkflowDetail_UploadFailed) = 2, InvalidMaterialSelection (Print3DWorkflowDetail_InvalidMaterialSelection) = 3, InvalidModel (Print3DWorkflowDetail_InvalidModel) = 4, ModelNotManifold (Print3DWorkflowDetail_ModelNotManifold) = 5, InvalidPrintTicket (Print3DWorkflowDetail_InvalidPrintTicket) = 6,
		}}
		DEFINE_IID!(IID_IPrint3DWorkflowPrintRequestedEventArgs, 435734616, 23240, 19285, 138, 95, 230, 21, 103, 218, 251, 77);
		RT_INTERFACE!{interface IPrint3DWorkflowPrintRequestedEventArgs(IPrint3DWorkflowPrintRequestedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPrint3DWorkflowPrintRequestedEventArgs] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::printers::extensions::Print3DWorkflowStatus) -> ::w::HRESULT,
			fn SetExtendedStatus(&mut self, value: ::rt::gen::windows::devices::printers::extensions::Print3DWorkflowDetail) -> ::w::HRESULT,
			fn SetSource(&mut self, source: *mut IInspectable) -> ::w::HRESULT,
			fn SetSourceChanged(&mut self, value: ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(Print3DWorkflowPrintRequestedEventArgs: ::rt::gen::windows::devices::printers::extensions::IPrint3DWorkflowPrintRequestedEventArgs);
		DEFINE_IID!(IID_IPrint3DWorkflow, 3312415933, 13929, 19046, 171, 66, 200, 21, 25, 48, 205, 52);
		RT_INTERFACE!{interface IPrint3DWorkflow(IPrint3DWorkflowVtbl): IInspectable(IInspectableVtbl) [IID_IPrint3DWorkflow] {
			fn get_DeviceID(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetPrintModelPackage(&mut self, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn get_IsPrintReady(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsPrintReady(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn add_PrintRequested(&mut self, eventHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::printers::extensions::Print3DWorkflow, &::rt::gen::windows::devices::printers::extensions::Print3DWorkflowPrintRequestedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_PrintRequested(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(Print3DWorkflow: ::rt::gen::windows::devices::printers::extensions::IPrint3DWorkflow);
		RT_STRUCT! { struct ExtensionsContract {
			
		}}
		DEFINE_IID!(IID_IPrintTaskConfigurationSaveRequestedDeferral, 3914978664, 63273, 17572, 135, 29, 189, 6, 40, 105, 106, 51);
		RT_INTERFACE!{interface IPrintTaskConfigurationSaveRequestedDeferral(IPrintTaskConfigurationSaveRequestedDeferralVtbl): IInspectable(IInspectableVtbl) [IID_IPrintTaskConfigurationSaveRequestedDeferral] {
			fn Complete(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(PrintTaskConfigurationSaveRequestedDeferral: ::rt::gen::windows::devices::printers::extensions::IPrintTaskConfigurationSaveRequestedDeferral);
		DEFINE_IID!(IID_IPrintTaskConfigurationSaveRequest, 4004458443, 25118, 19298, 172, 119, 178, 129, 204, 224, 141, 96);
		RT_INTERFACE!{interface IPrintTaskConfigurationSaveRequest(IPrintTaskConfigurationSaveRequestVtbl): IInspectable(IInspectableVtbl) [IID_IPrintTaskConfigurationSaveRequest] {
			fn Cancel(&mut self) -> ::w::HRESULT,
			fn Save(&mut self, printerExtensionContext: *mut IInspectable) -> ::w::HRESULT,
			fn GetDeferral(&mut self, out: *mut *mut ::rt::gen::windows::devices::printers::extensions::PrintTaskConfigurationSaveRequestedDeferral) -> ::w::HRESULT,
			fn get_Deadline(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT
		}}
		RT_CLASS!(PrintTaskConfigurationSaveRequest: ::rt::gen::windows::devices::printers::extensions::IPrintTaskConfigurationSaveRequest);
		DEFINE_IID!(IID_IPrintTaskConfigurationSaveRequestedEventArgs, 3765184633, 3425, 18744, 145, 208, 150, 164, 91, 238, 132, 121);
		RT_INTERFACE!{interface IPrintTaskConfigurationSaveRequestedEventArgs(IPrintTaskConfigurationSaveRequestedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPrintTaskConfigurationSaveRequestedEventArgs] {
			fn get_Request(&mut self, out: *mut *mut ::rt::gen::windows::devices::printers::extensions::PrintTaskConfigurationSaveRequest) -> ::w::HRESULT
		}}
		RT_CLASS!(PrintTaskConfigurationSaveRequestedEventArgs: ::rt::gen::windows::devices::printers::extensions::IPrintTaskConfigurationSaveRequestedEventArgs);
		DEFINE_IID!(IID_IPrintTaskConfiguration, 3821151313, 15012, 18565, 146, 64, 49, 31, 95, 143, 190, 157);
		RT_INTERFACE!{interface IPrintTaskConfiguration(IPrintTaskConfigurationVtbl): IInspectable(IInspectableVtbl) [IID_IPrintTaskConfiguration] {
			fn get_PrinterExtensionContext(&mut self, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn add_SaveRequested(&mut self, eventHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::printers::extensions::PrintTaskConfiguration, &::rt::gen::windows::devices::printers::extensions::PrintTaskConfigurationSaveRequestedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SaveRequested(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(PrintTaskConfiguration: ::rt::gen::windows::devices::printers::extensions::IPrintTaskConfiguration);
		DEFINE_IID!(IID_IPrintNotificationEventDetails, 3759033482, 18472, 19873, 139, 184, 134, 114, 223, 133, 21, 231);
		RT_INTERFACE!{interface IPrintNotificationEventDetails(IPrintNotificationEventDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IPrintNotificationEventDetails] {
			fn get_PrinterName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_EventData(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_EventData(&mut self, value: ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(PrintNotificationEventDetails: ::rt::gen::windows::devices::printers::extensions::IPrintNotificationEventDetails);
		DEFINE_IID!(IID_IPrintExtensionContextStatic, 3876429761, 65401, 19108, 140, 155, 12, 147, 174, 223, 222, 138);
		RT_INTERFACE!{interface IPrintExtensionContextStatic(IPrintExtensionContextStaticVtbl): IInspectable(IInspectableVtbl) [IID_IPrintExtensionContextStatic] {
			fn FromDeviceId(&mut self, deviceId: ::w::HSTRING, out: *mut *mut IInspectable) -> ::w::HRESULT
		}}
} // Windows.Devices.Printers.Extensions
} // Windows.Devices.Printers
pub mod power { // Windows.Devices.Power
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_IBattery, 3163115462, 114, 18376, 139, 93, 97, 74, 170, 122, 67, 126);
		RT_INTERFACE!{interface IBattery(IBatteryVtbl): IInspectable(IInspectableVtbl) [IID_IBattery] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetReport(&mut self, out: *mut *mut ::rt::gen::windows::devices::power::BatteryReport) -> ::w::HRESULT,
			fn add_ReportUpdated(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::power::Battery, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReportUpdated(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(BatteryReport: ::rt::gen::windows::devices::power::IBatteryReport);
		RT_CLASS!(Battery: ::rt::gen::windows::devices::power::IBattery);
		DEFINE_IID!(IID_IBatteryReport, 3380972602, 19987, 16906, 168, 208, 36, 241, 143, 57, 84, 1);
		RT_INTERFACE!{interface IBatteryReport(IBatteryReportVtbl): IInspectable(IInspectableVtbl) [IID_IBatteryReport] {
			fn get_ChargeRateInMilliwatts(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<i32>) -> ::w::HRESULT,
			fn get_DesignCapacityInMilliwattHours(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<i32>) -> ::w::HRESULT,
			fn get_FullChargeCapacityInMilliwattHours(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<i32>) -> ::w::HRESULT,
			fn get_RemainingCapacityInMilliwattHours(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<i32>) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::system::power::BatteryStatus) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBatteryStatics, 2043507382, 40542, 17490, 190, 166, 223, 205, 84, 30, 89, 127);
		RT_INTERFACE!{interface IBatteryStatics(IBatteryStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBatteryStatics] {
			fn get_AggregateBattery(&mut self, out: *mut *mut ::rt::gen::windows::devices::power::Battery) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::power::Battery>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
} // Windows.Devices.Power
pub mod sms { // Windows.Devices.Sms
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum SmsMessageClass: i32 {
			None (SmsMessageClass_None) = 0, Class0 (SmsMessageClass_Class0) = 1, Class1 (SmsMessageClass_Class1) = 2, Class2 (SmsMessageClass_Class2) = 3, Class3 (SmsMessageClass_Class3) = 4,
		}}
		RT_ENUM! { enum SmsMessageType: i32 {
			Binary (SmsMessageType_Binary) = 0, Text (SmsMessageType_Text) = 1, Wap (SmsMessageType_Wap) = 2, App (SmsMessageType_App) = 3, Broadcast (SmsMessageType_Broadcast) = 4, Voicemail (SmsMessageType_Voicemail) = 5, Status (SmsMessageType_Status) = 6,
		}}
		RT_ENUM! { enum CellularClass: i32 {
			None (CellularClass_None) = 0, Gsm (CellularClass_Gsm) = 1, Cdma (CellularClass_Cdma) = 2,
		}}
		DEFINE_IID!(IID_ISmsMessageBase, 753991216, 65104, 20422, 170, 136, 76, 207, 226, 122, 41, 234);
		RT_INTERFACE!{interface ISmsMessageBase(ISmsMessageBaseVtbl): IInspectable(IInspectableVtbl) [IID_ISmsMessageBase] {
			fn get_MessageType(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsMessageType) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_CellularClass(&mut self, out: *mut ::rt::gen::windows::devices::sms::CellularClass) -> ::w::HRESULT,
			fn get_MessageClass(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsMessageClass) -> ::w::HRESULT,
			fn get_SimIccId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_ENUM! { enum SmsDataFormat: i32 {
			Unknown (SmsDataFormat_Unknown) = 0, CdmaSubmit (SmsDataFormat_CdmaSubmit) = 1, GsmSubmit (SmsDataFormat_GsmSubmit) = 2, CdmaDeliver (SmsDataFormat_CdmaDeliver) = 3, GsmDeliver (SmsDataFormat_GsmDeliver) = 4,
		}}
		RT_ENUM! { enum SmsEncoding: i32 {
			Unknown (SmsEncoding_Unknown) = 0, Optimal (SmsEncoding_Optimal) = 1, SevenBitAscii (SmsEncoding_SevenBitAscii) = 2, Unicode (SmsEncoding_Unicode) = 3, GsmSevenBit (SmsEncoding_GsmSevenBit) = 4, EightBit (SmsEncoding_EightBit) = 5, Latin (SmsEncoding_Latin) = 6, Korean (SmsEncoding_Korean) = 7, IA5 (SmsEncoding_IA5) = 8, ShiftJis (SmsEncoding_ShiftJis) = 9, LatinHebrew (SmsEncoding_LatinHebrew) = 10,
		}}
		DEFINE_IID!(IID_ISmsTextMessage2, 580966547, 17749, 18261, 181, 161, 231, 253, 132, 149, 95, 141);
		RT_INTERFACE!{interface ISmsTextMessage2(ISmsTextMessage2Vtbl): IInspectable(IInspectableVtbl) [IID_ISmsTextMessage2] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_To(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_To(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_From(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Body(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_Body(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_Encoding(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsEncoding) -> ::w::HRESULT,
			fn put_Encoding(&mut self, value: ::rt::gen::windows::devices::sms::SmsEncoding) -> ::w::HRESULT,
			fn get_CallbackNumber(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_CallbackNumber(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsDeliveryNotificationEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsDeliveryNotificationEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_RetryAttemptCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_RetryAttemptCount(&mut self, value: i32) -> ::w::HRESULT,
			fn get_TeleserviceId(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ProtocolId(&mut self, out: *mut i32) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsTextMessage2: ::rt::gen::windows::devices::sms::ISmsTextMessage2);
		DEFINE_IID!(IID_ISmsWapMessage, 3448993603, 31317, 19771, 144, 33, 242, 46, 2, 45, 9, 197);
		RT_INTERFACE!{interface ISmsWapMessage(ISmsWapMessageVtbl): IInspectable(IInspectableVtbl) [IID_ISmsWapMessage] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_To(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_From(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ApplicationId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ContentType(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_BinaryBody(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_Headers(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMap<&str, &str>) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsWapMessage: ::rt::gen::windows::devices::sms::ISmsWapMessage);
		DEFINE_IID!(IID_ISmsAppMessage, 3904603284, 54176, 18954, 134, 215, 41, 16, 51, 168, 207, 84);
		RT_INTERFACE!{interface ISmsAppMessage(ISmsAppMessageVtbl): IInspectable(IInspectableVtbl) [IID_ISmsAppMessage] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_To(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_To(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_From(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Body(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_Body(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_CallbackNumber(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_CallbackNumber(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsDeliveryNotificationEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsDeliveryNotificationEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_RetryAttemptCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_RetryAttemptCount(&mut self, value: i32) -> ::w::HRESULT,
			fn get_Encoding(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsEncoding) -> ::w::HRESULT,
			fn put_Encoding(&mut self, value: ::rt::gen::windows::devices::sms::SmsEncoding) -> ::w::HRESULT,
			fn get_PortNumber(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_PortNumber(&mut self, value: i32) -> ::w::HRESULT,
			fn get_TeleserviceId(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_TeleserviceId(&mut self, value: i32) -> ::w::HRESULT,
			fn get_ProtocolId(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_ProtocolId(&mut self, value: i32) -> ::w::HRESULT,
			fn get_BinaryBody(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_BinaryBody(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsAppMessage: ::rt::gen::windows::devices::sms::ISmsAppMessage);
		RT_ENUM! { enum SmsGeographicalScope: i32 {
			None (SmsGeographicalScope_None) = 0, CellWithImmediateDisplay (SmsGeographicalScope_CellWithImmediateDisplay) = 1, LocationArea (SmsGeographicalScope_LocationArea) = 2, Plmn (SmsGeographicalScope_Plmn) = 3, Cell (SmsGeographicalScope_Cell) = 4,
		}}
		RT_ENUM! { enum SmsBroadcastType: i32 {
			Other (SmsBroadcastType_Other) = 0, CmasPresidential (SmsBroadcastType_CmasPresidential) = 1, CmasExtreme (SmsBroadcastType_CmasExtreme) = 2, CmasSevere (SmsBroadcastType_CmasSevere) = 3, CmasAmber (SmsBroadcastType_CmasAmber) = 4, CmasTest (SmsBroadcastType_CmasTest) = 5, EUAlert1 (SmsBroadcastType_EUAlert1) = 6, EUAlert2 (SmsBroadcastType_EUAlert2) = 7, EUAlert3 (SmsBroadcastType_EUAlert3) = 8, EUAlertAmber (SmsBroadcastType_EUAlertAmber) = 9, EUAlertInfo (SmsBroadcastType_EUAlertInfo) = 10, EtwsEarthquake (SmsBroadcastType_EtwsEarthquake) = 11, EtwsTsunami (SmsBroadcastType_EtwsTsunami) = 12, EtwsTsunamiAndEarthquake (SmsBroadcastType_EtwsTsunamiAndEarthquake) = 13, LatAlertLocal (SmsBroadcastType_LatAlertLocal) = 14,
		}}
		DEFINE_IID!(IID_ISmsBroadcastMessage, 1974385649, 58551, 18548, 160, 156, 41, 86, 229, 146, 249, 87);
		RT_INTERFACE!{interface ISmsBroadcastMessage(ISmsBroadcastMessageVtbl): IInspectable(IInspectableVtbl) [IID_ISmsBroadcastMessage] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_To(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Body(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Channel(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_GeographicalScope(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsGeographicalScope) -> ::w::HRESULT,
			fn get_MessageCode(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_UpdateNumber(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_BroadcastType(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsBroadcastType) -> ::w::HRESULT,
			fn get_IsEmergencyAlert(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsUserPopupRequested(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsBroadcastMessage: ::rt::gen::windows::devices::sms::ISmsBroadcastMessage);
		DEFINE_IID!(IID_ISmsVoicemailMessage, 656056486, 38321, 17663, 188, 184, 184, 253, 215, 224, 139, 195);
		RT_INTERFACE!{interface ISmsVoicemailMessage(ISmsVoicemailMessageVtbl): IInspectable(IInspectableVtbl) [IID_ISmsVoicemailMessage] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_To(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Body(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_MessageCount(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<i32>) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsVoicemailMessage: ::rt::gen::windows::devices::sms::ISmsVoicemailMessage);
		DEFINE_IID!(IID_ISmsStatusMessage, 3872555842, 46859, 18039, 147, 121, 201, 120, 63, 223, 248, 244);
		RT_INTERFACE!{interface ISmsStatusMessage(ISmsStatusMessageVtbl): IInspectable(IInspectableVtbl) [IID_ISmsStatusMessage] {
			fn get_To(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_From(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Body(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MessageReferenceNumber(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ServiceCenterTimestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_DischargeTime(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsStatusMessage: ::rt::gen::windows::devices::sms::ISmsStatusMessage);
		RT_STRUCT! { struct SmsEncodedLength {
			SegmentCount: u32, CharacterCountLastSegment: u32, CharactersPerSegment: u32, ByteCountLastSegment: u32, BytesPerSegment: u32,
		}}
		RT_ENUM! { enum SmsDeviceStatus: i32 {
			Off (SmsDeviceStatus_Off) = 0, Ready (SmsDeviceStatus_Ready) = 1, SimNotInserted (SmsDeviceStatus_SimNotInserted) = 2, BadSim (SmsDeviceStatus_BadSim) = 3, DeviceFailure (SmsDeviceStatus_DeviceFailure) = 4, SubscriptionNotActivated (SmsDeviceStatus_SubscriptionNotActivated) = 5, DeviceLocked (SmsDeviceStatus_DeviceLocked) = 6, DeviceBlocked (SmsDeviceStatus_DeviceBlocked) = 7,
		}}
		RT_ENUM! { enum SmsModemErrorCode: i32 {
			Other (SmsModemErrorCode_Other) = 0, MessagingNetworkError (SmsModemErrorCode_MessagingNetworkError) = 1, SmsOperationNotSupportedByDevice (SmsModemErrorCode_SmsOperationNotSupportedByDevice) = 2, SmsServiceNotSupportedByNetwork (SmsModemErrorCode_SmsServiceNotSupportedByNetwork) = 3, DeviceFailure (SmsModemErrorCode_DeviceFailure) = 4, MessageNotEncodedProperly (SmsModemErrorCode_MessageNotEncodedProperly) = 5, MessageTooLarge (SmsModemErrorCode_MessageTooLarge) = 6, DeviceNotReady (SmsModemErrorCode_DeviceNotReady) = 7, NetworkNotReady (SmsModemErrorCode_NetworkNotReady) = 8, InvalidSmscAddress (SmsModemErrorCode_InvalidSmscAddress) = 9, NetworkFailure (SmsModemErrorCode_NetworkFailure) = 10, FixedDialingNumberRestricted (SmsModemErrorCode_FixedDialingNumberRestricted) = 11,
		}}
		DEFINE_IID!(IID_ISmsSendMessageResult, 3675495154, 30921, 20459, 150, 34, 69, 35, 40, 8, 141, 98);
		RT_INTERFACE!{interface ISmsSendMessageResult(ISmsSendMessageResultVtbl): IInspectable(IInspectableVtbl) [IID_ISmsSendMessageResult] {
			fn get_IsSuccessful(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_MessageReferenceNumbers(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<i32>) -> ::w::HRESULT,
			fn get_CellularClass(&mut self, out: *mut ::rt::gen::windows::devices::sms::CellularClass) -> ::w::HRESULT,
			fn get_ModemErrorCode(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsModemErrorCode) -> ::w::HRESULT,
			fn get_IsErrorTransient(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_NetworkCauseCode(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_TransportFailureCause(&mut self, out: *mut i32) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsSendMessageResult: ::rt::gen::windows::devices::sms::ISmsSendMessageResult);
		DEFINE_IID!(IID_ISmsDevice2Statics, 1707574053, 4145, 18718, 143, 182, 239, 153, 145, 175, 227, 99);
		RT_INTERFACE!{interface ISmsDevice2Statics(ISmsDevice2StaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISmsDevice2Statics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromId(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::sms::SmsDevice2) -> ::w::HRESULT,
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsDevice2) -> ::w::HRESULT,
			fn FromParentId(&mut self, parentDeviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::sms::SmsDevice2) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsDevice2: ::rt::gen::windows::devices::sms::ISmsDevice2);
		DEFINE_IID!(IID_ISmsDevice2, 3179961363, 58658, 18123, 184, 213, 158, 173, 48, 251, 108, 71);
		RT_INTERFACE!{interface ISmsDevice2(ISmsDevice2Vtbl): IInspectable(IInspectableVtbl) [IID_ISmsDevice2] {
			fn get_SmscAddress(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_SmscAddress(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ParentDeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_AccountPhoneNumber(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_CellularClass(&mut self, out: *mut ::rt::gen::windows::devices::sms::CellularClass) -> ::w::HRESULT,
			fn get_DeviceStatus(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsDeviceStatus) -> ::w::HRESULT,
			fn CalculateLength(&mut self, message: *mut ::rt::gen::windows::devices::sms::ISmsMessageBase, out: *mut ::rt::gen::windows::devices::sms::SmsEncodedLength) -> ::w::HRESULT,
			fn SendMessageAndGetResultAsync(&mut self, message: *mut ::rt::gen::windows::devices::sms::ISmsMessageBase, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sms::SmsSendMessageResult>) -> ::w::HRESULT,
			fn add_DeviceStatusChanged(&mut self, eventHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sms::SmsDevice2, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_DeviceStatusChanged(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsMessageReceivedTriggerDetails, 735038420, 9815, 16680, 173, 95, 227, 135, 113, 50, 189, 177);
		RT_INTERFACE!{interface ISmsMessageReceivedTriggerDetails(ISmsMessageReceivedTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_ISmsMessageReceivedTriggerDetails] {
			fn get_MessageType(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsMessageType) -> ::w::HRESULT,
			fn get_TextMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsTextMessage2) -> ::w::HRESULT,
			fn get_WapMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsWapMessage) -> ::w::HRESULT,
			fn get_AppMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsAppMessage) -> ::w::HRESULT,
			fn get_BroadcastMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsBroadcastMessage) -> ::w::HRESULT,
			fn get_VoicemailMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsVoicemailMessage) -> ::w::HRESULT,
			fn get_StatusMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsStatusMessage) -> ::w::HRESULT,
			fn Drop(&mut self) -> ::w::HRESULT,
			fn Accept(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsMessageReceivedTriggerDetails: ::rt::gen::windows::devices::sms::ISmsMessageReceivedTriggerDetails);
		RT_ENUM! { enum SmsFilterActionType: i32 {
			AcceptImmediately (SmsFilterActionType_AcceptImmediately) = 0, Drop (SmsFilterActionType_Drop) = 1, Peek (SmsFilterActionType_Peek) = 2, Accept (SmsFilterActionType_Accept) = 3,
		}}
		DEFINE_IID!(IID_ISmsFilterRule, 1088630702, 45129, 20412, 175, 233, 226, 166, 16, 239, 245, 92);
		RT_INTERFACE!{interface ISmsFilterRule(ISmsFilterRuleVtbl): IInspectable(IInspectableVtbl) [IID_ISmsFilterRule] {
			fn get_MessageType(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsMessageType) -> ::w::HRESULT,
			fn get_ImsiPrefixes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn get_DeviceIds(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn get_SenderNumbers(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn get_TextMessagePrefixes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn get_PortNumbers(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<i32>) -> ::w::HRESULT,
			fn get_CellularClass(&mut self, out: *mut ::rt::gen::windows::devices::sms::CellularClass) -> ::w::HRESULT,
			fn put_CellularClass(&mut self, value: ::rt::gen::windows::devices::sms::CellularClass) -> ::w::HRESULT,
			fn get_ProtocolIds(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<i32>) -> ::w::HRESULT,
			fn get_TeleserviceIds(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<i32>) -> ::w::HRESULT,
			fn get_WapApplicationIds(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn get_WapContentTypes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn get_BroadcastTypes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<::rt::gen::windows::devices::sms::SmsBroadcastType>) -> ::w::HRESULT,
			fn get_BroadcastChannels(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<i32>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsFilterRuleFactory, 12805384, 25238, 20265, 154, 173, 137, 32, 206, 186, 60, 232);
		RT_INTERFACE!{interface ISmsFilterRuleFactory(ISmsFilterRuleFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ISmsFilterRuleFactory] {
			fn CreateFilterRule(&mut self, messageType: ::rt::gen::windows::devices::sms::SmsMessageType, out: *mut *mut ::rt::gen::windows::devices::sms::SmsFilterRule) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsFilterRule: ::rt::gen::windows::devices::sms::ISmsFilterRule);
		DEFINE_IID!(IID_ISmsFilterRules, 1313336059, 31181, 18561, 152, 148, 85, 164, 19, 91, 35, 250);
		RT_INTERFACE!{interface ISmsFilterRules(ISmsFilterRulesVtbl): IInspectable(IInspectableVtbl) [IID_ISmsFilterRules] {
			fn get_ActionType(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsFilterActionType) -> ::w::HRESULT,
			fn get_Rules(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::sms::SmsFilterRule>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsFilterRulesFactory, 2694391021, 28206, 17712, 159, 222, 70, 93, 2, 238, 208, 14);
		RT_INTERFACE!{interface ISmsFilterRulesFactory(ISmsFilterRulesFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ISmsFilterRulesFactory] {
			fn CreateFilterRules(&mut self, actionType: ::rt::gen::windows::devices::sms::SmsFilterActionType, out: *mut *mut ::rt::gen::windows::devices::sms::SmsFilterRules) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsFilterRules: ::rt::gen::windows::devices::sms::ISmsFilterRules);
		DEFINE_IID!(IID_ISmsMessageRegistrationStatics, 1671451748, 10392, 18296, 160, 60, 111, 153, 73, 7, 214, 58);
		RT_INTERFACE!{interface ISmsMessageRegistrationStatics(ISmsMessageRegistrationStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISmsMessageRegistrationStatics] {
			fn get_AllRegistrations(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sms::SmsMessageRegistration>) -> ::w::HRESULT,
			fn Register(&mut self, id: ::w::HSTRING, filterRules: *mut ::rt::gen::windows::devices::sms::SmsFilterRules, out: *mut *mut ::rt::gen::windows::devices::sms::SmsMessageRegistration) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsMessageRegistration: ::rt::gen::windows::devices::sms::ISmsMessageRegistration);
		DEFINE_IID!(IID_ISmsMessageRegistration, 387993662, 62287, 17515, 131, 179, 15, 241, 153, 35, 180, 9);
		RT_INTERFACE!{interface ISmsMessageRegistration(ISmsMessageRegistrationVtbl): IInspectable(IInspectableVtbl) [IID_ISmsMessageRegistration] {
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn Unregister(&mut self) -> ::w::HRESULT,
			fn add_MessageReceived(&mut self, eventHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sms::SmsMessageRegistration, &::rt::gen::windows::devices::sms::SmsMessageReceivedTriggerDetails>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_MessageReceived(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_STRUCT! { struct LegacySmsApiContract {
			
		}}
		DEFINE_IID!(IID_ISmsMessage, 3980156456, 27012, 19207, 129, 29, 141, 89, 6, 237, 60, 234);
		RT_INTERFACE!{interface ISmsMessage(ISmsMessageVtbl): IInspectable(IInspectableVtbl) [IID_ISmsMessage] {
			fn get_Id(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_MessageClass(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsMessageClass) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsBinaryMessage, 1542776851, 15187, 19566, 182, 26, 216, 106, 99, 117, 86, 80);
		RT_INTERFACE!{interface ISmsBinaryMessage(ISmsBinaryMessageVtbl): IInspectable(IInspectableVtbl) [IID_ISmsBinaryMessage] {
			fn get_Format(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsDataFormat) -> ::w::HRESULT,
			fn put_Format(&mut self, value: ::rt::gen::windows::devices::sms::SmsDataFormat) -> ::w::HRESULT,
			fn GetData(&mut self, out: *mut *mut u8) -> ::w::HRESULT,
			fn SetData(&mut self, value: *mut u8) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsBinaryMessage: ::rt::gen::windows::devices::sms::ISmsBinaryMessage);
		DEFINE_IID!(IID_ISmsTextMessage, 3592196172, 42133, 18559, 154, 111, 151, 21, 72, 197, 188, 159);
		RT_INTERFACE!{interface ISmsTextMessage(ISmsTextMessageVtbl): IInspectable(IInspectableVtbl) [IID_ISmsTextMessage] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_PartReferenceId(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_PartNumber(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_PartCount(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_To(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_To(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_From(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_From(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_Body(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_Body(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_Encoding(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsEncoding) -> ::w::HRESULT,
			fn put_Encoding(&mut self, value: ::rt::gen::windows::devices::sms::SmsEncoding) -> ::w::HRESULT,
			fn ToBinaryMessages(&mut self, format: ::rt::gen::windows::devices::sms::SmsDataFormat, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sms::ISmsBinaryMessage>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsTextMessageStatics, 2137572845, 15564, 18339, 140, 85, 56, 13, 59, 1, 8, 146);
		RT_INTERFACE!{interface ISmsTextMessageStatics(ISmsTextMessageStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISmsTextMessageStatics] {
			fn FromBinaryMessage(&mut self, binaryMessage: *mut ::rt::gen::windows::devices::sms::SmsBinaryMessage, out: *mut *mut ::rt::gen::windows::devices::sms::SmsTextMessage) -> ::w::HRESULT,
			fn FromBinaryData(&mut self, format: ::rt::gen::windows::devices::sms::SmsDataFormat, value: *mut u8, out: *mut *mut ::rt::gen::windows::devices::sms::SmsTextMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsTextMessage: ::rt::gen::windows::devices::sms::ISmsTextMessage);
		RT_ENUM! { enum SmsMessageFilter: i32 {
			All (SmsMessageFilter_All) = 0, Unread (SmsMessageFilter_Unread) = 1, Read (SmsMessageFilter_Read) = 2, Sent (SmsMessageFilter_Sent) = 3, Draft (SmsMessageFilter_Draft) = 4,
		}}
		RT_CLASS!(DeleteSmsMessageOperation: ::rt::gen::windows::foundation::IAsyncAction);
		RT_CLASS!(DeleteSmsMessagesOperation: ::rt::gen::windows::foundation::IAsyncAction);
		RT_CLASS!(GetSmsMessageOperation: ::rt::gen::windows::foundation::IAsyncOperation<&'static ::rt::gen::windows::devices::sms::ISmsMessage>);
		RT_CLASS!(GetSmsMessagesOperation: ::rt::gen::windows::foundation::IAsyncOperationWithProgress<&'static ::rt::gen::windows::foundation::collections::IVectorView<&'static ::rt::gen::windows::devices::sms::ISmsMessage>, i32>);
		DEFINE_IID!(IID_ISmsDeviceMessageStore, 2559177299, 61832, 17447, 141, 84, 206, 12, 36, 35, 197, 193);
		RT_INTERFACE!{interface ISmsDeviceMessageStore(ISmsDeviceMessageStoreVtbl): IInspectable(IInspectableVtbl) [IID_ISmsDeviceMessageStore] {
			fn DeleteMessageAsync(&mut self, messageId: u32, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn DeleteMessagesAsync(&mut self, messageFilter: ::rt::gen::windows::devices::sms::SmsMessageFilter, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn GetMessageAsync(&mut self, messageId: u32, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sms::ISmsMessage>) -> ::w::HRESULT,
			fn GetMessagesAsync(&mut self, messageFilter: ::rt::gen::windows::devices::sms::SmsMessageFilter, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperationWithProgress<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sms::ISmsMessage>, i32>) -> ::w::HRESULT,
			fn get_MaxMessages(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsDeviceMessageStore: ::rt::gen::windows::devices::sms::ISmsDeviceMessageStore);
		RT_CLASS!(SendSmsMessageOperation: ::rt::gen::windows::foundation::IAsyncAction);
		DEFINE_IID!(IID_ISmsMessageReceivedEventArgs, 149424792, 47333, 16833, 163, 216, 211, 171, 250, 226, 38, 117);
		RT_INTERFACE!{interface ISmsMessageReceivedEventArgs(ISmsMessageReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ISmsMessageReceivedEventArgs] {
			fn get_TextMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsTextMessage) -> ::w::HRESULT,
			fn get_BinaryMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsBinaryMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsMessageReceivedEventArgs: ::rt::gen::windows::devices::sms::ISmsMessageReceivedEventArgs);
		DEFINE_IID!(IID_SmsMessageReceivedEventHandler, 192599049, 60461, 18382, 162, 83, 115, 43, 238, 235, 202, 205);
		RT_DELEGATE!{delegate SmsMessageReceivedEventHandler(SmsMessageReceivedEventHandlerVtbl, SmsMessageReceivedEventHandlerImpl) [IID_SmsMessageReceivedEventHandler] {
			fn Invoke(&mut self, sender: *mut ::rt::gen::windows::devices::sms::SmsDevice, e: *mut ::rt::gen::windows::devices::sms::SmsMessageReceivedEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsDevice: ::rt::gen::windows::devices::sms::ISmsDevice);
		DEFINE_IID!(IID_SmsDeviceStatusChangedEventHandler, 2552959330, 15831, 17944, 175, 137, 12, 39, 45, 93, 6, 216);
		RT_DELEGATE!{delegate SmsDeviceStatusChangedEventHandler(SmsDeviceStatusChangedEventHandlerVtbl, SmsDeviceStatusChangedEventHandlerImpl) [IID_SmsDeviceStatusChangedEventHandler] {
			fn Invoke(&mut self, sender: *mut ::rt::gen::windows::devices::sms::SmsDevice) -> ::w::HRESULT
		}}
		RT_CLASS!(GetSmsDeviceOperation: ::rt::gen::windows::foundation::IAsyncOperation<&'static ::rt::gen::windows::devices::sms::SmsDevice>);
		DEFINE_IID!(IID_ISmsDeviceStatics, 4169992170, 55317, 19921, 162, 52, 69, 32, 206, 70, 4, 164);
		RT_INTERFACE!{interface ISmsDeviceStatics(ISmsDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISmsDeviceStatics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sms::SmsDevice>) -> ::w::HRESULT,
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sms::SmsDevice>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsDeviceStatics2, 748756103, 2163, 19631, 138, 125, 189, 71, 30, 133, 134, 209);
		RT_INTERFACE!{interface ISmsDeviceStatics2(ISmsDeviceStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_ISmsDeviceStatics2] {
			fn FromNetworkAccountIdAsync(&mut self, networkAccountId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sms::SmsDevice>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsDevice, 152539629, 34603, 20204, 156, 114, 171, 17, 98, 123, 52, 236);
		RT_INTERFACE!{interface ISmsDevice(ISmsDeviceVtbl): IInspectable(IInspectableVtbl) [IID_ISmsDevice] {
			fn SendMessageAsync(&mut self, message: *mut ::rt::gen::windows::devices::sms::ISmsMessage, out: *mut *mut ::rt::gen::windows::devices::sms::SendSmsMessageOperation) -> ::w::HRESULT,
			fn CalculateLength(&mut self, message: *mut ::rt::gen::windows::devices::sms::SmsTextMessage, out: *mut ::rt::gen::windows::devices::sms::SmsEncodedLength) -> ::w::HRESULT,
			fn get_AccountPhoneNumber(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_CellularClass(&mut self, out: *mut ::rt::gen::windows::devices::sms::CellularClass) -> ::w::HRESULT,
			fn get_MessageStore(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsDeviceMessageStore) -> ::w::HRESULT,
			fn get_DeviceStatus(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsDeviceStatus) -> ::w::HRESULT,
			fn add_SmsMessageReceived(&mut self, eventHandler: *mut ::rt::gen::windows::devices::sms::SmsMessageReceivedEventHandler, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SmsMessageReceived(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_SmsDeviceStatusChanged(&mut self, eventHandler: *mut ::rt::gen::windows::devices::sms::SmsDeviceStatusChangedEventHandler, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SmsDeviceStatusChanged(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsReceivedEventDetails, 1538592533, 58477, 19586, 132, 125, 90, 3, 4, 193, 213, 61);
		RT_INTERFACE!{interface ISmsReceivedEventDetails(ISmsReceivedEventDetailsVtbl): IInspectable(IInspectableVtbl) [IID_ISmsReceivedEventDetails] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_MessageIndex(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmsReceivedEventDetails2, 1088445574, 42932, 18289, 154, 231, 11, 95, 251, 18, 192, 58);
		RT_INTERFACE!{interface ISmsReceivedEventDetails2(ISmsReceivedEventDetails2Vtbl): IInspectable(IInspectableVtbl) [IID_ISmsReceivedEventDetails2] {
			fn get_MessageClass(&mut self, out: *mut ::rt::gen::windows::devices::sms::SmsMessageClass) -> ::w::HRESULT,
			fn get_BinaryMessage(&mut self, out: *mut *mut ::rt::gen::windows::devices::sms::SmsBinaryMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(SmsReceivedEventDetails: ::rt::gen::windows::devices::sms::ISmsReceivedEventDetails);
} // Windows.Devices.Sms
pub mod alljoyn { // Windows.Devices.AllJoyn
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum AllJoynAuthenticationMechanism: i32 {
			None (AllJoynAuthenticationMechanism_None) = 0, SrpAnonymous (AllJoynAuthenticationMechanism_SrpAnonymous) = 1, SrpLogon (AllJoynAuthenticationMechanism_SrpLogon) = 2, EcdheNull (AllJoynAuthenticationMechanism_EcdheNull) = 3, EcdhePsk (AllJoynAuthenticationMechanism_EcdhePsk) = 4, EcdheEcdsa (AllJoynAuthenticationMechanism_EcdheEcdsa) = 5,
		}}
		DEFINE_IID!(IID_IAllJoynStatusStatics, 3501695358, 3369, 19881, 138, 198, 84, 197, 84, 190, 219, 197);
		RT_INTERFACE!{interface IAllJoynStatusStatics(IAllJoynStatusStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynStatusStatics] {
			fn get_Ok(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Fail(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_OperationTimedOut(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_OtherEndClosed(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ConnectionRefused(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_AuthenticationFailed(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_AuthenticationRejectedByUser(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_SslConnectFailed(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_SslIdentityVerificationFailed(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InsufficientSecurity(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InvalidArgument1(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InvalidArgument2(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InvalidArgument3(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InvalidArgument4(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InvalidArgument5(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InvalidArgument6(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InvalidArgument7(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_InvalidArgument8(&mut self, out: *mut i32) -> ::w::HRESULT
		}}
		RT_ENUM! { enum AllJoynBusAttachmentState: i32 {
			Disconnected (AllJoynBusAttachmentState_Disconnected) = 0, Connecting (AllJoynBusAttachmentState_Connecting) = 1, Connected (AllJoynBusAttachmentState_Connected) = 2, Disconnecting (AllJoynBusAttachmentState_Disconnecting) = 3,
		}}
		DEFINE_IID!(IID_IAllJoynBusAttachment, 4077515091, 7917, 17091, 162, 14, 67, 109, 65, 254, 98, 246);
		RT_INTERFACE!{interface IAllJoynBusAttachment(IAllJoynBusAttachmentVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynBusAttachment] {
			fn get_AboutData(&mut self, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynAboutData) -> ::w::HRESULT,
			fn get_ConnectionSpecification(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_State(&mut self, out: *mut ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachmentState) -> ::w::HRESULT,
			fn get_UniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn PingAsync(&mut self, uniqueName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<i32>) -> ::w::HRESULT,
			fn Connect(&mut self) -> ::w::HRESULT,
			fn Disconnect(&mut self) -> ::w::HRESULT,
			fn add_StateChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, &::rt::gen::windows::devices::alljoyn::AllJoynBusAttachmentStateChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StateChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_AuthenticationMechanisms(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationMechanism>) -> ::w::HRESULT,
			fn add_CredentialsRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, &::rt::gen::windows::devices::alljoyn::AllJoynCredentialsRequestedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_CredentialsRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_CredentialsVerificationRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, &::rt::gen::windows::devices::alljoyn::AllJoynCredentialsVerificationRequestedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_CredentialsVerificationRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_AuthenticationComplete(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, &::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationCompleteEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AuthenticationComplete(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynAboutData: ::rt::gen::windows::devices::alljoyn::IAllJoynAboutData);
		RT_CLASS!(AllJoynBusAttachment: ::rt::gen::windows::devices::alljoyn::IAllJoynBusAttachment);
		RT_CLASS!(AllJoynBusAttachmentStateChangedEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynBusAttachmentStateChangedEventArgs);
		RT_CLASS!(AllJoynCredentialsRequestedEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynCredentialsRequestedEventArgs);
		RT_CLASS!(AllJoynCredentialsVerificationRequestedEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynCredentialsVerificationRequestedEventArgs);
		RT_CLASS!(AllJoynAuthenticationCompleteEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynAuthenticationCompleteEventArgs);
		DEFINE_IID!(IID_IAllJoynBusAttachmentStateChangedEventArgs, 3626923508, 49194, 16876, 168, 213, 234, 177, 85, 137, 83, 170);
		RT_INTERFACE!{interface IAllJoynBusAttachmentStateChangedEventArgs(IAllJoynBusAttachmentStateChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynBusAttachmentStateChangedEventArgs] {
			fn get_State(&mut self, out: *mut ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachmentState) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut i32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynCredentials, 2185646322, 41360, 16561, 171, 171, 52, 158, 194, 68, 223, 170);
		RT_INTERFACE!{interface IAllJoynCredentials(IAllJoynCredentialsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynCredentials] {
			fn get_AuthenticationMechanism(&mut self, out: *mut ::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationMechanism) -> ::w::HRESULT,
			fn get_Certificate(&mut self, out: *mut *mut ::rt::gen::windows::security::cryptography::certificates::Certificate) -> ::w::HRESULT,
			fn put_Certificate(&mut self, value: *mut ::rt::gen::windows::security::cryptography::certificates::Certificate) -> ::w::HRESULT,
			fn get_PasswordCredential(&mut self, out: *mut *mut ::rt::gen::windows::security::credentials::PasswordCredential) -> ::w::HRESULT,
			fn put_PasswordCredential(&mut self, value: *mut ::rt::gen::windows::security::credentials::PasswordCredential) -> ::w::HRESULT,
			fn get_Timeout(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn put_Timeout(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynCredentials: ::rt::gen::windows::devices::alljoyn::IAllJoynCredentials);
		DEFINE_IID!(IID_IAllJoynCredentialsRequestedEventArgs, 1787290446, 45161, 19328, 158, 26, 65, 188, 131, 124, 101, 210);
		RT_INTERFACE!{interface IAllJoynCredentialsRequestedEventArgs(IAllJoynCredentialsRequestedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynCredentialsRequestedEventArgs] {
			fn get_AttemptCount(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Credentials(&mut self, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynCredentials) -> ::w::HRESULT,
			fn get_PeerUniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_RequestedUserName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeferral(&mut self, out: *mut *mut ::rt::gen::windows::foundation::Deferral) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynCredentialsVerificationRequestedEventArgs, 2148169234, 47109, 17583, 162, 225, 121, 42, 182, 85, 162, 208);
		RT_INTERFACE!{interface IAllJoynCredentialsVerificationRequestedEventArgs(IAllJoynCredentialsVerificationRequestedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynCredentialsVerificationRequestedEventArgs] {
			fn get_AuthenticationMechanism(&mut self, out: *mut ::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationMechanism) -> ::w::HRESULT,
			fn get_PeerUniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_PeerCertificate(&mut self, out: *mut *mut ::rt::gen::windows::security::cryptography::certificates::Certificate) -> ::w::HRESULT,
			fn get_PeerCertificateErrorSeverity(&mut self, out: *mut ::rt::gen::windows::networking::sockets::SocketSslErrorSeverity) -> ::w::HRESULT,
			fn get_PeerCertificateErrors(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<::rt::gen::windows::security::cryptography::certificates::ChainValidationResult>) -> ::w::HRESULT,
			fn get_PeerIntermediateCertificates(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::security::cryptography::certificates::Certificate>) -> ::w::HRESULT,
			fn Accept(&mut self) -> ::w::HRESULT,
			fn GetDeferral(&mut self, out: *mut *mut ::rt::gen::windows::foundation::Deferral) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynAuthenticationCompleteEventArgs, 2545184796, 5596, 19283, 182, 164, 125, 19, 67, 0, 215, 191);
		RT_INTERFACE!{interface IAllJoynAuthenticationCompleteEventArgs(IAllJoynAuthenticationCompleteEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynAuthenticationCompleteEventArgs] {
			fn get_AuthenticationMechanism(&mut self, out: *mut ::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationMechanism) -> ::w::HRESULT,
			fn get_PeerUniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Succeeded(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynBusAttachmentFactory, 1680798116, 44421, 19935, 144, 174, 96, 68, 82, 178, 34, 136);
		RT_INTERFACE!{interface IAllJoynBusAttachmentFactory(IAllJoynBusAttachmentFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynBusAttachmentFactory] {
			fn Create(&mut self, connectionSpecification: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynServiceInfo, 1287553545, 47422, 16770, 153, 155, 221, 208, 0, 249, 197, 117);
		RT_INTERFACE!{interface IAllJoynServiceInfo(IAllJoynServiceInfoVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynServiceInfo] {
			fn get_UniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ObjectPath(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SessionPort(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynServiceInfoFactory, 1971444413, 65027, 20299, 148, 164, 240, 47, 220, 189, 17, 184);
		RT_INTERFACE!{interface IAllJoynServiceInfoFactory(IAllJoynServiceInfoFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynServiceInfoFactory] {
			fn Create(&mut self, uniqueName: ::w::HSTRING, objectPath: ::w::HSTRING, sessionPort: u16, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynServiceInfo) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynServiceInfo: ::rt::gen::windows::devices::alljoyn::IAllJoynServiceInfo);
		DEFINE_IID!(IID_IAllJoynAboutDataViewStatics, 1475196552, 3166, 16750, 136, 181, 57, 179, 45, 37, 196, 125);
		RT_INTERFACE!{interface IAllJoynAboutDataViewStatics(IAllJoynAboutDataViewStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynAboutDataViewStatics] {
			fn GetDataBySessionPortAsync(&mut self, uniqueName: ::w::HSTRING, busAttachment: *mut ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, sessionPort: u16, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::alljoyn::AllJoynAboutDataView>) -> ::w::HRESULT,
			fn GetDataBySessionPortWithLanguageAsync(&mut self, uniqueName: ::w::HSTRING, busAttachment: *mut ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, sessionPort: u16, language: *mut ::rt::gen::windows::globalization::Language, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::alljoyn::AllJoynAboutDataView>) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynAboutDataView: ::rt::gen::windows::devices::alljoyn::IAllJoynAboutDataView);
		DEFINE_IID!(IID_IAllJoynAboutDataView, 1747128607, 25106, 18740, 156, 72, 225, 156, 164, 152, 66, 136);
		RT_INTERFACE!{interface IAllJoynAboutDataView(IAllJoynAboutDataViewVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynAboutDataView] {
			fn get_Status(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT,
			fn get_AJSoftwareVersion(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_AppId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_DateOfManufacture(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::DateTime>) -> ::w::HRESULT,
			fn get_DefaultLanguage(&mut self, out: *mut *mut ::rt::gen::windows::globalization::Language) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_HardwareVersion(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ModelNumber(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SoftwareVersion(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SupportedLanguages(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::globalization::Language>) -> ::w::HRESULT,
			fn get_SupportUrl(&mut self, out: *mut *mut ::rt::gen::windows::foundation::Uri) -> ::w::HRESULT,
			fn get_AppName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Description(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DeviceName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Manufacturer(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynAboutData, 3853106944, 8098, 18489, 147, 239, 249, 223, 64, 72, 144, 247);
		RT_INTERFACE!{interface IAllJoynAboutData(IAllJoynAboutDataVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynAboutData] {
			fn get_IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_DefaultAppName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_DefaultAppName(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_AppNames(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMap<&str, &str>) -> ::w::HRESULT,
			fn get_DateOfManufacture(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::DateTime>) -> ::w::HRESULT,
			fn put_DateOfManufacture(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::DateTime>) -> ::w::HRESULT,
			fn get_DefaultDescription(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_DefaultDescription(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_Descriptions(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMap<&str, &str>) -> ::w::HRESULT,
			fn get_DefaultManufacturer(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_DefaultManufacturer(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_Manufacturers(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMap<&str, &str>) -> ::w::HRESULT,
			fn get_ModelNumber(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_ModelNumber(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_SoftwareVersion(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_SoftwareVersion(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_SupportUrl(&mut self, out: *mut *mut ::rt::gen::windows::foundation::Uri) -> ::w::HRESULT,
			fn put_SupportUrl(&mut self, value: *mut ::rt::gen::windows::foundation::Uri) -> ::w::HRESULT,
			fn get_AppId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn put_AppId(&mut self, value: ::w::GUID) -> ::w::HRESULT
		}}
		RT_ENUM! { enum AllJoynTrafficType: i32 {
			Unknown (AllJoynTrafficType_Unknown) = 0, Messages (AllJoynTrafficType_Messages) = 1, RawUnreliable (AllJoynTrafficType_RawUnreliable) = 2, RawReliable (AllJoynTrafficType_RawReliable) = 4,
		}}
		DEFINE_IID!(IID_IAllJoynAcceptSessionJoinerEventArgs, 1325093733, 16010, 16983, 143, 16, 83, 156, 224, 213, 108, 15);
		RT_INTERFACE!{interface IAllJoynAcceptSessionJoinerEventArgs(IAllJoynAcceptSessionJoinerEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynAcceptSessionJoinerEventArgs] {
			fn get_UniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SessionPort(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_TrafficType(&mut self, out: *mut ::rt::gen::windows::devices::alljoyn::AllJoynTrafficType) -> ::w::HRESULT,
			fn get_SamePhysicalNode(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_SameNetwork(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn Accept(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynAcceptSessionJoiner, 1302861778, 52509, 16419, 167, 196, 22, 222, 248, 156, 40, 223);
		RT_INTERFACE!{interface IAllJoynAcceptSessionJoiner(IAllJoynAcceptSessionJoinerVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynAcceptSessionJoiner] {
			fn Accept(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynAcceptSessionJoinerEventArgsFactory, 3024313280, 24901, 17054, 132, 219, 213, 191, 231, 114, 177, 79);
		RT_INTERFACE!{interface IAllJoynAcceptSessionJoinerEventArgsFactory(IAllJoynAcceptSessionJoinerEventArgsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynAcceptSessionJoinerEventArgsFactory] {
			fn Create(&mut self, uniqueName: ::w::HSTRING, sessionPort: u16, trafficType: ::rt::gen::windows::devices::alljoyn::AllJoynTrafficType, proximity: u8, acceptSessionJoiner: *mut ::rt::gen::windows::devices::alljoyn::IAllJoynAcceptSessionJoiner, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynAcceptSessionJoinerEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynAcceptSessionJoinerEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynAcceptSessionJoinerEventArgs);
		DEFINE_IID!(IID_IAllJoynSessionMemberAddedEventArgs, 1235384714, 3537, 18113, 156, 214, 39, 25, 14, 80, 58, 94);
		RT_INTERFACE!{interface IAllJoynSessionMemberAddedEventArgs(IAllJoynSessionMemberAddedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynSessionMemberAddedEventArgs] {
			fn get_UniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynSessionMemberAddedEventArgsFactory, 874373970, 7475, 16545, 161, 211, 229, 119, 112, 32, 225, 241);
		RT_INTERFACE!{interface IAllJoynSessionMemberAddedEventArgsFactory(IAllJoynSessionMemberAddedEventArgsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynSessionMemberAddedEventArgsFactory] {
			fn Create(&mut self, uniqueName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynSessionMemberAddedEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynSessionMemberAddedEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynSessionMemberAddedEventArgs);
		DEFINE_IID!(IID_IAllJoynSessionMemberRemovedEventArgs, 1083842975, 43594, 18579, 180, 48, 186, 161, 182, 60, 98, 25);
		RT_INTERFACE!{interface IAllJoynSessionMemberRemovedEventArgs(IAllJoynSessionMemberRemovedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynSessionMemberRemovedEventArgs] {
			fn get_UniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynSessionMemberRemovedEventArgsFactory, 3302184424, 17080, 19303, 183, 87, 208, 207, 202, 213, 146, 128);
		RT_INTERFACE!{interface IAllJoynSessionMemberRemovedEventArgsFactory(IAllJoynSessionMemberRemovedEventArgsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynSessionMemberRemovedEventArgsFactory] {
			fn Create(&mut self, uniqueName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynSessionMemberRemovedEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynSessionMemberRemovedEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynSessionMemberRemovedEventArgs);
		RT_ENUM! { enum AllJoynSessionLostReason: i32 {
			None (AllJoynSessionLostReason_None) = 0, ProducerLeftSession (AllJoynSessionLostReason_ProducerLeftSession) = 1, ProducerClosedAbruptly (AllJoynSessionLostReason_ProducerClosedAbruptly) = 2, RemovedByProducer (AllJoynSessionLostReason_RemovedByProducer) = 3, LinkTimeout (AllJoynSessionLostReason_LinkTimeout) = 4, Other (AllJoynSessionLostReason_Other) = 5,
		}}
		DEFINE_IID!(IID_IAllJoynSessionLostEventArgs, 3882263690, 35768, 18772, 174, 103, 210, 250, 67, 209, 249, 107);
		RT_INTERFACE!{interface IAllJoynSessionLostEventArgs(IAllJoynSessionLostEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynSessionLostEventArgs] {
			fn get_Reason(&mut self, out: *mut ::rt::gen::windows::devices::alljoyn::AllJoynSessionLostReason) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynSessionLostEventArgsFactory, 331087154, 54004, 18889, 152, 14, 40, 5, 225, 53, 134, 177);
		RT_INTERFACE!{interface IAllJoynSessionLostEventArgsFactory(IAllJoynSessionLostEventArgsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynSessionLostEventArgsFactory] {
			fn Create(&mut self, reason: ::rt::gen::windows::devices::alljoyn::AllJoynSessionLostReason, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynSessionLostEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynSessionLostEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynSessionLostEventArgs);
		DEFINE_IID!(IID_IAllJoynProducerStoppedEventArgs, 1362138992, 18743, 18733, 128, 128, 35, 100, 57, 152, 124, 235);
		RT_INTERFACE!{interface IAllJoynProducerStoppedEventArgs(IAllJoynProducerStoppedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynProducerStoppedEventArgs] {
			fn get_Status(&mut self, out: *mut i32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynProducerStoppedEventArgsFactory, 1448253793, 45593, 19822, 159, 120, 250, 63, 153, 250, 143, 229);
		RT_INTERFACE!{interface IAllJoynProducerStoppedEventArgsFactory(IAllJoynProducerStoppedEventArgsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynProducerStoppedEventArgsFactory] {
			fn Create(&mut self, status: i32, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynProducerStoppedEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynProducerStoppedEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynProducerStoppedEventArgs);
		DEFINE_IID!(IID_IAllJoynWatcherStoppedEventArgs, 3388776507, 28701, 19112, 151, 221, 162, 187, 10, 143, 95, 163);
		RT_INTERFACE!{interface IAllJoynWatcherStoppedEventArgs(IAllJoynWatcherStoppedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynWatcherStoppedEventArgs] {
			fn get_Status(&mut self, out: *mut i32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynWatcherStoppedEventArgsFactory, 2274338216, 11600, 18401, 144, 74, 32, 191, 13, 72, 199, 130);
		RT_INTERFACE!{interface IAllJoynWatcherStoppedEventArgsFactory(IAllJoynWatcherStoppedEventArgsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynWatcherStoppedEventArgsFactory] {
			fn Create(&mut self, status: i32, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynWatcherStoppedEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynWatcherStoppedEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynWatcherStoppedEventArgs);
		DEFINE_IID!(IID_IAllJoynServiceInfoRemovedEventArgs, 811051359, 7487, 16883, 137, 105, 227, 39, 146, 98, 115, 150);
		RT_INTERFACE!{interface IAllJoynServiceInfoRemovedEventArgs(IAllJoynServiceInfoRemovedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynServiceInfoRemovedEventArgs] {
			fn get_UniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynServiceInfoRemovedEventArgsFactory, 230655527, 39679, 18773, 146, 39, 105, 83, 186, 244, 21, 105);
		RT_INTERFACE!{interface IAllJoynServiceInfoRemovedEventArgsFactory(IAllJoynServiceInfoRemovedEventArgsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynServiceInfoRemovedEventArgsFactory] {
			fn Create(&mut self, uniqueName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynServiceInfoRemovedEventArgs) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynServiceInfoRemovedEventArgs: ::rt::gen::windows::devices::alljoyn::IAllJoynServiceInfoRemovedEventArgs);
		DEFINE_IID!(IID_IAllJoynMessageInfo, 4281008423, 11282, 18521, 170, 58, 199, 68, 97, 238, 129, 76);
		RT_INTERFACE!{interface IAllJoynMessageInfo(IAllJoynMessageInfoVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynMessageInfo] {
			fn get_SenderUniqueName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAllJoynMessageInfoFactory, 879119402, 33417, 17364, 180, 168, 63, 77, 227, 89, 240, 67);
		RT_INTERFACE!{interface IAllJoynMessageInfoFactory(IAllJoynMessageInfoFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IAllJoynMessageInfoFactory] {
			fn Create(&mut self, senderUniqueName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::alljoyn::AllJoynMessageInfo) -> ::w::HRESULT
		}}
		RT_CLASS!(AllJoynMessageInfo: ::rt::gen::windows::devices::alljoyn::IAllJoynMessageInfo);
} // Windows.Devices.AllJoyn
pub mod background { // Windows.Devices.Background
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_IDeviceUseDetails, 2102808897, 21886, 16724, 185, 148, 228, 247, 161, 31, 179, 35);
		RT_INTERFACE!{interface IDeviceUseDetails(IDeviceUseDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceUseDetails] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Arguments(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceUseDetails: ::rt::gen::windows::devices::background::IDeviceUseDetails);
		DEFINE_IID!(IID_IDeviceServicingDetails, 1252781609, 9028, 19140, 133, 39, 74, 142, 246, 144, 86, 69);
		RT_INTERFACE!{interface IDeviceServicingDetails(IDeviceServicingDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceServicingDetails] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Arguments(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ExpectedDuration(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceServicingDetails: ::rt::gen::windows::devices::background::IDeviceServicingDetails);
} // Windows.Devices.Background
pub mod bluetooth { // Windows.Devices.Bluetooth
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum BluetoothCacheMode: i32 {
			Cached (BluetoothCacheMode_Cached) = 0, Uncached (BluetoothCacheMode_Uncached) = 1,
		}}
		RT_ENUM! { enum BluetoothMajorClass: i32 {
			Miscellaneous (BluetoothMajorClass_Miscellaneous) = 0, Computer (BluetoothMajorClass_Computer) = 1, Phone (BluetoothMajorClass_Phone) = 2, NetworkAccessPoint (BluetoothMajorClass_NetworkAccessPoint) = 3, AudioVideo (BluetoothMajorClass_AudioVideo) = 4, Peripheral (BluetoothMajorClass_Peripheral) = 5, Imaging (BluetoothMajorClass_Imaging) = 6, Wearable (BluetoothMajorClass_Wearable) = 7, Toy (BluetoothMajorClass_Toy) = 8, Health (BluetoothMajorClass_Health) = 9,
		}}
		RT_ENUM! { enum BluetoothMinorClass: i32 {
			Uncategorized (BluetoothMinorClass_Uncategorized) = 0, ComputerDesktop (BluetoothMinorClass_ComputerDesktop) = 1, ComputerServer (BluetoothMinorClass_ComputerServer) = 2, ComputerLaptop (BluetoothMinorClass_ComputerLaptop) = 3, ComputerHandheld (BluetoothMinorClass_ComputerHandheld) = 4, ComputerPalmSize (BluetoothMinorClass_ComputerPalmSize) = 5, ComputerWearable (BluetoothMinorClass_ComputerWearable) = 6, ComputerTablet (BluetoothMinorClass_ComputerTablet) = 7, PhoneCellular (BluetoothMinorClass_PhoneCellular) = 1, PhoneCordless (BluetoothMinorClass_PhoneCordless) = 2, PhoneSmartPhone (BluetoothMinorClass_PhoneSmartPhone) = 3, PhoneWired (BluetoothMinorClass_PhoneWired) = 4, PhoneIsdn (BluetoothMinorClass_PhoneIsdn) = 5, NetworkFullyAvailable (BluetoothMinorClass_NetworkFullyAvailable) = 0, NetworkUsed01To17Percent (BluetoothMinorClass_NetworkUsed01To17Percent) = 8, NetworkUsed17To33Percent (BluetoothMinorClass_NetworkUsed17To33Percent) = 16, NetworkUsed33To50Percent (BluetoothMinorClass_NetworkUsed33To50Percent) = 24, NetworkUsed50To67Percent (BluetoothMinorClass_NetworkUsed50To67Percent) = 32, NetworkUsed67To83Percent (BluetoothMinorClass_NetworkUsed67To83Percent) = 40, NetworkUsed83To99Percent (BluetoothMinorClass_NetworkUsed83To99Percent) = 48, NetworkNoServiceAvailable (BluetoothMinorClass_NetworkNoServiceAvailable) = 56, AudioVideoWearableHeadset (BluetoothMinorClass_AudioVideoWearableHeadset) = 1, AudioVideoHandsFree (BluetoothMinorClass_AudioVideoHandsFree) = 2, AudioVideoMicrophone (BluetoothMinorClass_AudioVideoMicrophone) = 4, AudioVideoLoudspeaker (BluetoothMinorClass_AudioVideoLoudspeaker) = 5, AudioVideoHeadphones (BluetoothMinorClass_AudioVideoHeadphones) = 6, AudioVideoPortableAudio (BluetoothMinorClass_AudioVideoPortableAudio) = 7, AudioVideoCarAudio (BluetoothMinorClass_AudioVideoCarAudio) = 8, AudioVideoSetTopBox (BluetoothMinorClass_AudioVideoSetTopBox) = 9, AudioVideoHifiAudioDevice (BluetoothMinorClass_AudioVideoHifiAudioDevice) = 10, AudioVideoVcr (BluetoothMinorClass_AudioVideoVcr) = 11, AudioVideoVideoCamera (BluetoothMinorClass_AudioVideoVideoCamera) = 12, AudioVideoCamcorder (BluetoothMinorClass_AudioVideoCamcorder) = 13, AudioVideoVideoMonitor (BluetoothMinorClass_AudioVideoVideoMonitor) = 14, AudioVideoVideoDisplayAndLoudspeaker (BluetoothMinorClass_AudioVideoVideoDisplayAndLoudspeaker) = 15, AudioVideoVideoConferencing (BluetoothMinorClass_AudioVideoVideoConferencing) = 16, AudioVideoGamingOrToy (BluetoothMinorClass_AudioVideoGamingOrToy) = 18, PeripheralJoystick (BluetoothMinorClass_PeripheralJoystick) = 1, PeripheralGamepad (BluetoothMinorClass_PeripheralGamepad) = 2, PeripheralRemoteControl (BluetoothMinorClass_PeripheralRemoteControl) = 3, PeripheralSensing (BluetoothMinorClass_PeripheralSensing) = 4, PeripheralDigitizerTablet (BluetoothMinorClass_PeripheralDigitizerTablet) = 5, PeripheralCardReader (BluetoothMinorClass_PeripheralCardReader) = 6, PeripheralDigitalPen (BluetoothMinorClass_PeripheralDigitalPen) = 7, PeripheralHandheldScanner (BluetoothMinorClass_PeripheralHandheldScanner) = 8, PeripheralHandheldGesture (BluetoothMinorClass_PeripheralHandheldGesture) = 9, WearableWristwatch (BluetoothMinorClass_WearableWristwatch) = 1, WearablePager (BluetoothMinorClass_WearablePager) = 2, WearableJacket (BluetoothMinorClass_WearableJacket) = 3, WearableHelmet (BluetoothMinorClass_WearableHelmet) = 4, WearableGlasses (BluetoothMinorClass_WearableGlasses) = 5, ToyRobot (BluetoothMinorClass_ToyRobot) = 1, ToyVehicle (BluetoothMinorClass_ToyVehicle) = 2, ToyDoll (BluetoothMinorClass_ToyDoll) = 3, ToyController (BluetoothMinorClass_ToyController) = 4, ToyGame (BluetoothMinorClass_ToyGame) = 5, HealthBloodPressureMonitor (BluetoothMinorClass_HealthBloodPressureMonitor) = 1, HealthThermometer (BluetoothMinorClass_HealthThermometer) = 2, HealthWeighingScale (BluetoothMinorClass_HealthWeighingScale) = 3, HealthGlucoseMeter (BluetoothMinorClass_HealthGlucoseMeter) = 4, HealthPulseOximeter (BluetoothMinorClass_HealthPulseOximeter) = 5, HealthHeartRateMonitor (BluetoothMinorClass_HealthHeartRateMonitor) = 6, HealthHealthDataDisplay (BluetoothMinorClass_HealthHealthDataDisplay) = 7, HealthStepCounter (BluetoothMinorClass_HealthStepCounter) = 8, HealthBodyCompositionAnalyzer (BluetoothMinorClass_HealthBodyCompositionAnalyzer) = 9, HealthPeakFlowMonitor (BluetoothMinorClass_HealthPeakFlowMonitor) = 10, HealthMedicationMonitor (BluetoothMinorClass_HealthMedicationMonitor) = 11, HealthKneeProsthesis (BluetoothMinorClass_HealthKneeProsthesis) = 12, HealthAnkleProsthesis (BluetoothMinorClass_HealthAnkleProsthesis) = 13, HealthGenericHealthManager (BluetoothMinorClass_HealthGenericHealthManager) = 14, HealthPersonalMobilityDevice (BluetoothMinorClass_HealthPersonalMobilityDevice) = 15,
		}}
		RT_ENUM! { enum BluetoothServiceCapabilities: u32 {
			None (BluetoothServiceCapabilities_None) = 0, LimitedDiscoverableMode (BluetoothServiceCapabilities_LimitedDiscoverableMode) = 1, PositioningService (BluetoothServiceCapabilities_PositioningService) = 8, NetworkingService (BluetoothServiceCapabilities_NetworkingService) = 16, RenderingService (BluetoothServiceCapabilities_RenderingService) = 32, CapturingService (BluetoothServiceCapabilities_CapturingService) = 64, ObjectTransferService (BluetoothServiceCapabilities_ObjectTransferService) = 128, AudioService (BluetoothServiceCapabilities_AudioService) = 256, TelephoneService (BluetoothServiceCapabilities_TelephoneService) = 512, InformationService (BluetoothServiceCapabilities_InformationService) = 1024,
		}}
		RT_ENUM! { enum BluetoothConnectionStatus: i32 {
			Disconnected (BluetoothConnectionStatus_Disconnected) = 0, Connected (BluetoothConnectionStatus_Connected) = 1,
		}}
		RT_ENUM! { enum BluetoothError: i32 {
			Success (BluetoothError_Success) = 0, RadioNotAvailable (BluetoothError_RadioNotAvailable) = 1, ResourceInUse (BluetoothError_ResourceInUse) = 2, DeviceNotConnected (BluetoothError_DeviceNotConnected) = 3, OtherError (BluetoothError_OtherError) = 4, DisabledByPolicy (BluetoothError_DisabledByPolicy) = 5, NotSupported (BluetoothError_NotSupported) = 6, DisabledByUser (BluetoothError_DisabledByUser) = 7,
		}}
		RT_ENUM! { enum BluetoothAddressType: i32 {
			Public (BluetoothAddressType_Public) = 0, Random (BluetoothAddressType_Random) = 1,
		}}
		DEFINE_IID!(IID_IBluetoothDevice, 590721366, 37074, 18948, 174, 245, 14, 32, 185, 230, 183, 7);
		RT_INTERFACE!{interface IBluetoothDevice(IBluetoothDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothDevice] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_HostName(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ClassOfDevice(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothClassOfDevice) -> ::w::HRESULT,
			fn get_SdpRecords(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT,
			fn get_RfcommServices(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::rfcomm::RfcommDeviceService>) -> ::w::HRESULT,
			fn get_ConnectionStatus(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothConnectionStatus) -> ::w::HRESULT,
			fn get_BluetoothAddress(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn add_NameChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::BluetoothDevice, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_NameChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_SdpRecordsChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::BluetoothDevice, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SdpRecordsChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ConnectionStatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::BluetoothDevice, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ConnectionStatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothClassOfDevice: ::rt::gen::windows::devices::bluetooth::IBluetoothClassOfDevice);
		RT_CLASS!(BluetoothDevice: ::rt::gen::windows::devices::bluetooth::IBluetoothDevice);
		DEFINE_IID!(IID_IBluetoothDevice2, 20183380, 45398, 19920, 177, 245, 193, 27, 195, 26, 81, 99);
		RT_INTERFACE!{interface IBluetoothDevice2(IBluetoothDevice2Vtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothDevice2] {
			fn get_DeviceInformation(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothDeviceStatics, 160554833, 22491, 18213, 187, 215, 132, 246, 67, 39, 236, 44);
		RT_INTERFACE!{interface IBluetoothDeviceStatics(IBluetoothDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothDeviceStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::BluetoothDevice>) -> ::w::HRESULT,
			fn FromHostNameAsync(&mut self, hostName: *mut ::rt::gen::windows::networking::HostName, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::BluetoothDevice>) -> ::w::HRESULT,
			fn FromBluetoothAddressAsync(&mut self, address: u64, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::BluetoothDevice>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothDeviceStatics2, 3265170991, 19988, 17527, 170, 27, 184, 180, 126, 91, 126, 206);
		RT_INTERFACE!{interface IBluetoothDeviceStatics2(IBluetoothDeviceStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothDeviceStatics2] {
			fn GetDeviceSelectorFromPairingState(&mut self, pairingState: ::w::BOOL, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromConnectionStatus(&mut self, connectionStatus: ::rt::gen::windows::devices::bluetooth::BluetoothConnectionStatus, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromDeviceName(&mut self, deviceName: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromBluetoothAddress(&mut self, bluetoothAddress: u64, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromClassOfDevice(&mut self, classOfDevice: *mut ::rt::gen::windows::devices::bluetooth::BluetoothClassOfDevice, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAppearanceCategoriesStatics, 1833784574, 1130, 16773, 170, 182, 130, 76, 240, 97, 8, 97);
		RT_INTERFACE!{interface IBluetoothLEAppearanceCategoriesStatics(IBluetoothLEAppearanceCategoriesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAppearanceCategoriesStatics] {
			fn get_Uncategorized(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Phone(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Computer(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Watch(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Clock(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Display(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_RemoteControl(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_EyeGlasses(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Tag(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Keyring(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_MediaPlayer(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_BarcodeScanner(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Thermometer(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_HeartRate(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_BloodPressure(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_HumanInterfaceDevice(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_GlucoseMeter(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_RunningWalking(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Cycling(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_PulseOximeter(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_WeightScale(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_OutdoorSportActivity(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAppearanceSubcategoriesStatics, 3850085894, 8516, 16730, 131, 18, 113, 204, 242, 145, 248, 209);
		RT_INTERFACE!{interface IBluetoothLEAppearanceSubcategoriesStatics(IBluetoothLEAppearanceSubcategoriesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAppearanceSubcategoriesStatics] {
			fn get_Generic(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_SportsWatch(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_ThermometerEar(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_HeartRateBelt(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_BloodPressureArm(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_BloodPressureWrist(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Keyboard(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Mouse(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Joystick(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Gamepad(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_DigitizerTablet(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_CardReader(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_DigitalPen(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_BarcodeScanner(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_RunningWalkingInShoe(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_RunningWalkingOnShoe(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_RunningWalkingOnHip(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_CyclingComputer(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_CyclingSpeedSensor(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_CyclingCadenceSensor(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_CyclingPowerSensor(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_CyclingSpeedCadenceSensor(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_OximeterFingertip(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_OximeterWristWorn(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_LocationDisplay(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_LocationNavigationDisplay(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_LocationPod(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_LocationNavigationPod(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAppearance, 1562409458, 26280, 16984, 152, 94, 2, 180, 217, 80, 159, 24);
		RT_INTERFACE!{interface IBluetoothLEAppearance(IBluetoothLEAppearanceVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAppearance] {
			fn get_RawValue(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Category(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_SubCategory(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAppearanceStatics, 2710814919, 17668, 20298, 155, 165, 205, 16, 84, 229, 224, 101);
		RT_INTERFACE!{interface IBluetoothLEAppearanceStatics(IBluetoothLEAppearanceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAppearanceStatics] {
			fn FromRawValue(&mut self, rawValue: u16, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothLEAppearance) -> ::w::HRESULT,
			fn FromParts(&mut self, appearanceCategory: u16, appearanceSubCategory: u16, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothLEAppearance) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAppearance: ::rt::gen::windows::devices::bluetooth::IBluetoothLEAppearance);
		DEFINE_IID!(IID_IBluetoothLEDevice, 3052285819, 19160, 17986, 172, 72, 128, 160, 181, 0, 232, 135);
		RT_INTERFACE!{interface IBluetoothLEDevice(IBluetoothLEDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEDevice] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_GattServices(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService>) -> ::w::HRESULT,
			fn get_ConnectionStatus(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothConnectionStatus) -> ::w::HRESULT,
			fn get_BluetoothAddress(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn GetGattService(&mut self, serviceUuid: ::w::GUID, out: *mut *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService) -> ::w::HRESULT,
			fn add_NameChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::BluetoothLEDevice, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_NameChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_GattServicesChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::BluetoothLEDevice, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_GattServicesChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ConnectionStatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::BluetoothLEDevice, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ConnectionStatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEDevice: ::rt::gen::windows::devices::bluetooth::IBluetoothLEDevice);
		DEFINE_IID!(IID_IBluetoothLEDevice2, 653288115, 31470, 19761, 186, 186, 177, 185, 119, 95, 89, 22);
		RT_INTERFACE!{interface IBluetoothLEDevice2(IBluetoothLEDevice2Vtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEDevice2] {
			fn get_DeviceInformation(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT,
			fn get_Appearance(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothLEAppearance) -> ::w::HRESULT,
			fn get_BluetoothAddressType(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothAddressType) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEDeviceStatics, 3369015833, 61622, 19440, 134, 137, 65, 48, 61, 226, 217, 244);
		RT_INTERFACE!{interface IBluetoothLEDeviceStatics(IBluetoothLEDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEDeviceStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::BluetoothLEDevice>) -> ::w::HRESULT,
			fn FromBluetoothAddressAsync(&mut self, bluetoothAddress: u64, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::BluetoothLEDevice>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEDeviceStatics2, 1595064427, 15276, 17384, 173, 22, 86, 50, 113, 189, 65, 194);
		RT_INTERFACE!{interface IBluetoothLEDeviceStatics2(IBluetoothLEDeviceStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEDeviceStatics2] {
			fn GetDeviceSelectorFromPairingState(&mut self, pairingState: ::w::BOOL, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromConnectionStatus(&mut self, connectionStatus: ::rt::gen::windows::devices::bluetooth::BluetoothConnectionStatus, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromDeviceName(&mut self, deviceName: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromBluetoothAddress(&mut self, bluetoothAddress: u64, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromBluetoothAddressWithBluetoothAddressType(&mut self, bluetoothAddress: u64, bluetoothAddressType: ::rt::gen::windows::devices::bluetooth::BluetoothAddressType, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromAppearance(&mut self, appearance: *mut ::rt::gen::windows::devices::bluetooth::BluetoothLEAppearance, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromBluetoothAddressWithBluetoothAddressTypeAsync(&mut self, bluetoothAddress: u64, bluetoothAddressType: ::rt::gen::windows::devices::bluetooth::BluetoothAddressType, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::BluetoothLEDevice>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothClassOfDevice, 3594527358, 55255, 18017, 148, 84, 101, 3, 156, 161, 122, 43);
		RT_INTERFACE!{interface IBluetoothClassOfDevice(IBluetoothClassOfDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothClassOfDevice] {
			fn get_RawValue(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_MajorClass(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothMajorClass) -> ::w::HRESULT,
			fn get_MinorClass(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothMinorClass) -> ::w::HRESULT,
			fn get_ServiceCapabilities(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothServiceCapabilities) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothClassOfDeviceStatics, 3831575997, 4002, 16748, 145, 180, 193, 228, 140, 160, 97, 193);
		RT_INTERFACE!{interface IBluetoothClassOfDeviceStatics(IBluetoothClassOfDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothClassOfDeviceStatics] {
			fn FromRawValue(&mut self, rawValue: u32, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothClassOfDevice) -> ::w::HRESULT,
			fn FromParts(&mut self, majorClass: ::rt::gen::windows::devices::bluetooth::BluetoothMajorClass, minorClass: ::rt::gen::windows::devices::bluetooth::BluetoothMinorClass, serviceCapabilities: ::rt::gen::windows::devices::bluetooth::BluetoothServiceCapabilities, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothClassOfDevice) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothSignalStrengthFilter, 3749409681, 27573, 19710, 144, 177, 93, 115, 36, 237, 207, 127);
		RT_INTERFACE!{interface IBluetoothSignalStrengthFilter(IBluetoothSignalStrengthFilterVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothSignalStrengthFilter] {
			fn get_InRangeThresholdInDBm(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<i16>) -> ::w::HRESULT,
			fn put_InRangeThresholdInDBm(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<i16>) -> ::w::HRESULT,
			fn get_OutOfRangeThresholdInDBm(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<i16>) -> ::w::HRESULT,
			fn put_OutOfRangeThresholdInDBm(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<i16>) -> ::w::HRESULT,
			fn get_OutOfRangeTimeout(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::TimeSpan>) -> ::w::HRESULT,
			fn put_OutOfRangeTimeout(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::TimeSpan>) -> ::w::HRESULT,
			fn get_SamplingInterval(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::TimeSpan>) -> ::w::HRESULT,
			fn put_SamplingInterval(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::TimeSpan>) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothSignalStrengthFilter: ::rt::gen::windows::devices::bluetooth::IBluetoothSignalStrengthFilter);
pub mod rfcomm { // Windows.Devices.Bluetooth.Rfcomm
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(RfcommDeviceService: ::rt::gen::windows::devices::bluetooth::rfcomm::IRfcommDeviceService);
		DEFINE_IID!(IID_IRfcommServiceIdStatics, 706191034, 43381, 18147, 181, 107, 8, 255, 215, 131, 165, 254);
		RT_INTERFACE!{interface IRfcommServiceIdStatics(IRfcommServiceIdStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommServiceIdStatics] {
			fn FromUuid(&mut self, uuid: ::w::GUID, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn FromShortId(&mut self, shortId: u32, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_SerialPort(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_ObexObjectPush(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_ObexFileTransfer(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_PhoneBookAccessPce(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_PhoneBookAccessPse(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_GenericFileTransfer(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT
		}}
		RT_CLASS!(RfcommServiceId: ::rt::gen::windows::devices::bluetooth::rfcomm::IRfcommServiceId);
		DEFINE_IID!(IID_IRfcommServiceId, 576885252, 32258, 16407, 129, 54, 218, 27, 106, 27, 155, 191);
		RT_INTERFACE!{interface IRfcommServiceId(IRfcommServiceIdVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommServiceId] {
			fn get_Uuid(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn AsShortId(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn AsString(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IRfcommDeviceServiceStatics, 2762033647, 25197, 16812, 178, 83, 135, 172, 92, 39, 226, 138);
		RT_INTERFACE!{interface IRfcommDeviceServiceStatics(IRfcommDeviceServiceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommDeviceServiceStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::rfcomm::RfcommDeviceService>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, serviceId: *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IRfcommDeviceService, 2927755039, 50593, 19520, 140, 40, 243, 239, 214, 144, 98, 243);
		RT_INTERFACE!{interface IRfcommDeviceService(IRfcommDeviceServiceVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommDeviceService] {
			fn get_ConnectionHostName(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_ConnectionServiceName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ServiceId(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_ProtectionLevel(&mut self, out: *mut ::rt::gen::windows::networking::sockets::SocketProtectionLevel) -> ::w::HRESULT,
			fn get_MaxProtectionLevel(&mut self, out: *mut ::rt::gen::windows::networking::sockets::SocketProtectionLevel) -> ::w::HRESULT,
			fn GetSdpRawAttributesAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IMapView<u32, &::rt::gen::windows::storage::streams::IBuffer>>) -> ::w::HRESULT,
			fn GetSdpRawAttributesWithCacheModeAsync(&mut self, cacheMode: ::rt::gen::windows::devices::bluetooth::BluetoothCacheMode, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IMapView<u32, &::rt::gen::windows::storage::streams::IBuffer>>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IRfcommDeviceService2, 1399647508, 60365, 18942, 191, 159, 64, 239, 198, 137, 178, 13);
		RT_INTERFACE!{interface IRfcommDeviceService2(IRfcommDeviceService2Vtbl): IInspectable(IInspectableVtbl) [IID_IRfcommDeviceService2] {
			fn get_Device(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothDevice) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IRfcommServiceProviderStatics, 2559083267, 27082, 16698, 132, 247, 67, 68, 199, 41, 41, 151);
		RT_INTERFACE!{interface IRfcommServiceProviderStatics(IRfcommServiceProviderStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommServiceProviderStatics] {
			fn CreateAsync(&mut self, serviceId: *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceProvider>) -> ::w::HRESULT
		}}
		RT_CLASS!(RfcommServiceProvider: ::rt::gen::windows::devices::bluetooth::rfcomm::IRfcommServiceProvider);
		DEFINE_IID!(IID_IRfcommServiceProvider, 3940285892, 45558, 17663, 159, 124, 231, 168, 42, 184, 104, 33);
		RT_INTERFACE!{interface IRfcommServiceProvider(IRfcommServiceProviderVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommServiceProvider] {
			fn get_ServiceId(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_SdpRawAttributes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMap<u32, &::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT,
			fn StartAdvertising(&mut self, listener: *mut ::rt::gen::windows::networking::sockets::StreamSocketListener) -> ::w::HRESULT,
			fn StopAdvertising(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IRfcommServiceProvider2, 1936449478, 15489, 19742, 186, 242, 221, 187, 129, 40, 69, 18);
		RT_INTERFACE!{interface IRfcommServiceProvider2(IRfcommServiceProvider2Vtbl): IInspectable(IInspectableVtbl) [IID_IRfcommServiceProvider2] {
			fn StartAdvertisingWithRadioDiscoverability(&mut self, listener: *mut ::rt::gen::windows::networking::sockets::StreamSocketListener, radioDiscoverable: ::w::BOOL) -> ::w::HRESULT
		}}
} // Windows.Devices.Bluetooth.Rfcomm
pub mod genericattributeprofile { // Windows.Devices.Bluetooth.GenericAttributeProfile
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(GattDeviceService: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::IGattDeviceService);
		RT_ENUM! { enum GattCharacteristicProperties: u32 {
			None (GattCharacteristicProperties_None) = 0, Broadcast (GattCharacteristicProperties_Broadcast) = 1, Read (GattCharacteristicProperties_Read) = 2, WriteWithoutResponse (GattCharacteristicProperties_WriteWithoutResponse) = 4, Write (GattCharacteristicProperties_Write) = 8, Notify (GattCharacteristicProperties_Notify) = 16, Indicate (GattCharacteristicProperties_Indicate) = 32, AuthenticatedSignedWrites (GattCharacteristicProperties_AuthenticatedSignedWrites) = 64, ExtendedProperties (GattCharacteristicProperties_ExtendedProperties) = 128, ReliableWrites (GattCharacteristicProperties_ReliableWrites) = 256, WritableAuxiliaries (GattCharacteristicProperties_WritableAuxiliaries) = 512,
		}}
		RT_ENUM! { enum GattClientCharacteristicConfigurationDescriptorValue: i32 {
			None (GattClientCharacteristicConfigurationDescriptorValue_None) = 0, Notify (GattClientCharacteristicConfigurationDescriptorValue_Notify) = 1, Indicate (GattClientCharacteristicConfigurationDescriptorValue_Indicate) = 2,
		}}
		RT_ENUM! { enum GattProtectionLevel: i32 {
			Plain (GattProtectionLevel_Plain) = 0, AuthenticationRequired (GattProtectionLevel_AuthenticationRequired) = 1, EncryptionRequired (GattProtectionLevel_EncryptionRequired) = 2, EncryptionAndAuthenticationRequired (GattProtectionLevel_EncryptionAndAuthenticationRequired) = 3,
		}}
		RT_ENUM! { enum GattWriteOption: i32 {
			WriteWithResponse (GattWriteOption_WriteWithResponse) = 0, WriteWithoutResponse (GattWriteOption_WriteWithoutResponse) = 1,
		}}
		RT_ENUM! { enum GattCommunicationStatus: i32 {
			Success (GattCommunicationStatus_Success) = 0, Unreachable (GattCommunicationStatus_Unreachable) = 1,
		}}
		DEFINE_IID!(IID_IGattDeviceServiceStatics, 426573858, 64173, 17884, 174, 91, 42, 195, 24, 78, 132, 219);
		RT_INTERFACE!{interface IGattDeviceServiceStatics(IGattDeviceServiceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGattDeviceServiceStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService>) -> ::w::HRESULT,
			fn GetDeviceSelectorFromUuid(&mut self, serviceUuid: ::w::GUID, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromShortId(&mut self, serviceShortId: u16, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn ConvertShortIdToUuid(&mut self, shortId: u16, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		RT_CLASS!(GattCharacteristic: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::IGattCharacteristic);
		DEFINE_IID!(IID_IGattCharacteristicStatics, 1506496707, 22836, 20328, 161, 152, 235, 134, 79, 164, 78, 107);
		RT_INTERFACE!{interface IGattCharacteristicStatics(IGattCharacteristicStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGattCharacteristicStatics] {
			fn ConvertShortIdToUuid(&mut self, shortId: u16, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattCharacteristic, 1506496705, 22836, 20328, 161, 152, 235, 134, 79, 164, 78, 107);
		RT_INTERFACE!{interface IGattCharacteristic(IGattCharacteristicVtbl): IInspectable(IInspectableVtbl) [IID_IGattCharacteristic] {
			fn GetDescriptors(&mut self, descriptorUuid: ::w::GUID, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDescriptor>) -> ::w::HRESULT,
			fn get_CharacteristicProperties(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristicProperties) -> ::w::HRESULT,
			fn get_ProtectionLevel(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattProtectionLevel) -> ::w::HRESULT,
			fn put_ProtectionLevel(&mut self, value: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattProtectionLevel) -> ::w::HRESULT,
			fn get_UserDescription(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Uuid(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_AttributeHandle(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_PresentationFormats(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattPresentationFormat>) -> ::w::HRESULT,
			fn ReadValueAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadResult>) -> ::w::HRESULT,
			fn ReadValueWithCacheModeAsync(&mut self, cacheMode: ::rt::gen::windows::devices::bluetooth::BluetoothCacheMode, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadResult>) -> ::w::HRESULT,
			fn WriteValueAsync(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus>) -> ::w::HRESULT,
			fn WriteValueWithOptionAsync(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer, writeOption: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattWriteOption, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus>) -> ::w::HRESULT,
			fn ReadClientCharacteristicConfigurationDescriptorAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadClientCharacteristicConfigurationDescriptorResult>) -> ::w::HRESULT,
			fn WriteClientCharacteristicConfigurationDescriptorAsync(&mut self, clientCharacteristicConfigurationDescriptorValue: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattClientCharacteristicConfigurationDescriptorValue, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus>) -> ::w::HRESULT,
			fn add_ValueChanged(&mut self, valueChangedHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic, &::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattValueChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ValueChanged(&mut self, valueChangedEventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(GattDescriptor: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::IGattDescriptor);
		RT_CLASS!(GattPresentationFormat: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::IGattPresentationFormat);
		RT_CLASS!(GattReadResult: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::IGattReadResult);
		RT_CLASS!(GattReadClientCharacteristicConfigurationDescriptorResult: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::IGattReadClientCharacteristicConfigurationDescriptorResult);
		RT_CLASS!(GattValueChangedEventArgs: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::IGattValueChangedEventArgs);
		DEFINE_IID!(IID_IGattCharacteristic2, 2920985976, 60422, 18276, 183, 128, 152, 53, 161, 211, 93, 110);
		RT_INTERFACE!{interface IGattCharacteristic2(IGattCharacteristic2Vtbl): IInspectable(IInspectableVtbl) [IID_IGattCharacteristic2] {
			fn get_Service(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService) -> ::w::HRESULT,
			fn GetAllDescriptors(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDescriptor>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattDescriptorStatics, 2449825581, 32900, 17220, 180, 194, 40, 77, 225, 154, 133, 6);
		RT_INTERFACE!{interface IGattDescriptorStatics(IGattDescriptorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGattDescriptorStatics] {
			fn ConvertShortIdToUuid(&mut self, shortId: u16, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattDescriptor, 2449825579, 32900, 17220, 180, 194, 40, 77, 225, 154, 133, 6);
		RT_INTERFACE!{interface IGattDescriptor(IGattDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IGattDescriptor] {
			fn get_ProtectionLevel(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattProtectionLevel) -> ::w::HRESULT,
			fn put_ProtectionLevel(&mut self, value: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattProtectionLevel) -> ::w::HRESULT,
			fn get_Uuid(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_AttributeHandle(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn ReadValueAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadResult>) -> ::w::HRESULT,
			fn ReadValueWithCacheModeAsync(&mut self, cacheMode: ::rt::gen::windows::devices::bluetooth::BluetoothCacheMode, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadResult>) -> ::w::HRESULT,
			fn WriteValueAsync(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattPresentationFormatStatics, 426573856, 64173, 17884, 174, 91, 42, 195, 24, 78, 132, 219);
		RT_INTERFACE!{interface IGattPresentationFormatStatics(IGattPresentationFormatStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGattPresentationFormatStatics] {
			fn get_BluetoothSigAssignedNumbers(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattPresentationFormatTypesStatics, 4210145802, 12474, 16540, 190, 247, 207, 251, 109, 3, 184, 251);
		RT_INTERFACE!{interface IGattPresentationFormatTypesStatics(IGattPresentationFormatTypesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGattPresentationFormatTypesStatics] {
			fn get_Boolean(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Bit2(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Nibble(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_UInt8(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_UInt12(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_UInt16(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_UInt24(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_UInt32(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_UInt48(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_UInt64(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_UInt128(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SInt8(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SInt12(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SInt16(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SInt24(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SInt32(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SInt48(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SInt64(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SInt128(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Float32(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Float64(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SFloat(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Float(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_DUInt16(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Utf8(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Utf16(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Struct(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattPresentationFormat, 426573857, 64173, 17884, 174, 91, 42, 195, 24, 78, 132, 219);
		RT_INTERFACE!{interface IGattPresentationFormat(IGattPresentationFormatVtbl): IInspectable(IInspectableVtbl) [IID_IGattPresentationFormat] {
			fn get_FormatType(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Exponent(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Unit(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Namespace(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Description(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattValueChangedEventArgs, 3525040980, 1763, 20184, 162, 99, 172, 250, 200, 186, 115, 19);
		RT_INTERFACE!{interface IGattValueChangedEventArgs(IGattValueChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IGattValueChangedEventArgs] {
			fn get_CharacteristicValue(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattServiceUuidsStatics, 1841655896, 39610, 17431, 184, 242, 220, 224, 22, 211, 78, 226);
		RT_INTERFACE!{interface IGattServiceUuidsStatics(IGattServiceUuidsStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGattServiceUuidsStatics] {
			fn get_Battery(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_BloodPressure(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CyclingSpeedAndCadence(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GenericAccess(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GenericAttribute(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_Glucose(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_HealthThermometer(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_HeartRate(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_RunningSpeedAndCadence(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattServiceUuidsStatics2, 3534656757, 15637, 20345, 156, 12, 234, 175, 166, 117, 21, 92);
		RT_INTERFACE!{interface IGattServiceUuidsStatics2(IGattServiceUuidsStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IGattServiceUuidsStatics2] {
			fn get_AlertNotification(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CurrentTime(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CyclingPower(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_DeviceInformation(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_HumanInterfaceDevice(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ImmediateAlert(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_LinkLoss(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_LocationAndNavigation(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_NextDstChange(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_PhoneAlertStatus(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ReferenceTimeUpdate(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ScanParameters(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TxPower(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattCharacteristicUuidsStatics, 1492796806, 45534, 18188, 183, 222, 13, 17, 255, 68, 244, 183);
		RT_INTERFACE!{interface IGattCharacteristicUuidsStatics(IGattCharacteristicUuidsStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGattCharacteristicUuidsStatics] {
			fn get_BatteryLevel(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_BloodPressureFeature(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_BloodPressureMeasurement(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_BodySensorLocation(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CscFeature(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CscMeasurement(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GlucoseFeature(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GlucoseMeasurement(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GlucoseMeasurementContext(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_HeartRateControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_HeartRateMeasurement(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_IntermediateCuffPressure(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_IntermediateTemperature(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_MeasurementInterval(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_RecordAccessControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_RscFeature(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_RscMeasurement(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_SCControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_SensorLocation(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TemperatureMeasurement(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TemperatureType(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattCharacteristicUuidsStatics2, 408269861, 54382, 18988, 156, 63, 237, 109, 234, 41, 231, 190);
		RT_INTERFACE!{interface IGattCharacteristicUuidsStatics2(IGattCharacteristicUuidsStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IGattCharacteristicUuidsStatics2] {
			fn get_AlertCategoryId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_AlertCategoryIdBitMask(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_AlertLevel(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_AlertNotificationControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_AlertStatus(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GapAppearance(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_BootKeyboardInputReport(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_BootKeyboardOutputReport(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_BootMouseInputReport(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CurrentTime(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CyclingPowerControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CyclingPowerFeature(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CyclingPowerMeasurement(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CyclingPowerVector(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_DateTime(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_DayDateTime(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_DayOfWeek(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GapDeviceName(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_DstOffset(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ExactTime256(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_FirmwareRevisionString(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_HardwareRevisionString(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_HidControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_HidInformation(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_Ieee1107320601RegulatoryCertificationDataList(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_LnControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_LnFeature(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_LocalTimeInformation(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_LocationAndSpeed(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ManufacturerNameString(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ModelNumberString(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_Navigation(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_NewAlert(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GapPeripheralPreferredConnectionParameters(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GapPeripheralPrivacyFlag(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_PnpId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_PositionQuality(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ProtocolMode(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GapReconnectionAddress(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ReferenceTimeInformation(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_Report(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ReportMap(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_RingerControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_RingerSetting(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ScanIntervalWindow(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ScanRefresh(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_SerialNumberString(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_GattServiceChanged(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_SoftwareRevisionString(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_SupportedNewAlertCategory(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_SupportUnreadAlertCategory(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_SystemId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TimeAccuracy(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TimeSource(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TimeUpdateControlPoint(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TimeUpdateState(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TimeWithDst(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TimeZone(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_TxPowerLevel(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_UnreadAlertStatus(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattDescriptorUuidsStatics, 2801296078, 40188, 17137, 145, 133, 255, 55, 183, 81, 129, 211);
		RT_INTERFACE!{interface IGattDescriptorUuidsStatics(IGattDescriptorUuidsStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGattDescriptorUuidsStatics] {
			fn get_CharacteristicAggregateFormat(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CharacteristicExtendedProperties(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CharacteristicPresentationFormat(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_CharacteristicUserDescription(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ClientCharacteristicConfiguration(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_ServerCharacteristicConfiguration(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattReliableWriteTransaction, 1671851783, 6890, 19532, 165, 15, 151, 186, 228, 116, 179, 72);
		RT_INTERFACE!{interface IGattReliableWriteTransaction(IGattReliableWriteTransactionVtbl): IInspectable(IInspectableVtbl) [IID_IGattReliableWriteTransaction] {
			fn WriteValue(&mut self, characteristic: *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn CommitAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattReadResult, 1671851784, 6890, 19532, 165, 15, 151, 186, 228, 116, 179, 72);
		RT_INTERFACE!{interface IGattReadResult(IGattReadResultVtbl): IInspectable(IInspectableVtbl) [IID_IGattReadResult] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus) -> ::w::HRESULT,
			fn get_Value(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattReadClientCharacteristicConfigurationDescriptorResult, 1671851785, 6890, 19532, 165, 15, 151, 186, 228, 116, 179, 72);
		RT_INTERFACE!{interface IGattReadClientCharacteristicConfigurationDescriptorResult(IGattReadClientCharacteristicConfigurationDescriptorResultVtbl): IInspectable(IInspectableVtbl) [IID_IGattReadClientCharacteristicConfigurationDescriptorResult] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus) -> ::w::HRESULT,
			fn get_ClientCharacteristicConfigurationDescriptor(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattClientCharacteristicConfigurationDescriptorValue) -> ::w::HRESULT
		}}
		RT_CLASS!(GattReliableWriteTransaction: ::rt::gen::windows::devices::bluetooth::genericattributeprofile::IGattReliableWriteTransaction);
		DEFINE_IID!(IID_IGattDeviceService, 2893773829, 45884, 18383, 153, 15, 107, 143, 85, 119, 223, 113);
		RT_INTERFACE!{interface IGattDeviceService(IGattDeviceServiceVtbl): IInspectable(IInspectableVtbl) [IID_IGattDeviceService] {
			fn GetCharacteristics(&mut self, characteristicUuid: ::w::GUID, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic>) -> ::w::HRESULT,
			fn GetIncludedServices(&mut self, serviceUuid: ::w::GUID, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService>) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Uuid(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn get_AttributeHandle(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGattDeviceService2, 4233384459, 2829, 18184, 186, 224, 159, 253, 148, 137, 188, 89);
		RT_INTERFACE!{interface IGattDeviceService2(IGattDeviceService2Vtbl): IInspectable(IInspectableVtbl) [IID_IGattDeviceService2] {
			fn get_Device(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothLEDevice) -> ::w::HRESULT,
			fn get_ParentServices(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService>) -> ::w::HRESULT,
			fn GetAllCharacteristics(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic>) -> ::w::HRESULT,
			fn GetAllIncludedServices(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService>) -> ::w::HRESULT
		}}
} // Windows.Devices.Bluetooth.GenericAttributeProfile
pub mod advertisement { // Windows.Devices.Bluetooth.Advertisement
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum BluetoothLEScanningMode: i32 {
			Passive (BluetoothLEScanningMode_Passive) = 0, Active (BluetoothLEScanningMode_Active) = 1,
		}}
		RT_ENUM! { enum BluetoothLEAdvertisementFlags: u32 {
			None (BluetoothLEAdvertisementFlags_None) = 0, LimitedDiscoverableMode (BluetoothLEAdvertisementFlags_LimitedDiscoverableMode) = 1, GeneralDiscoverableMode (BluetoothLEAdvertisementFlags_GeneralDiscoverableMode) = 2, ClassicNotSupported (BluetoothLEAdvertisementFlags_ClassicNotSupported) = 4, DualModeControllerCapable (BluetoothLEAdvertisementFlags_DualModeControllerCapable) = 8, DualModeHostCapable (BluetoothLEAdvertisementFlags_DualModeHostCapable) = 16,
		}}
		RT_ENUM! { enum BluetoothLEAdvertisementType: i32 {
			ConnectableUndirected (BluetoothLEAdvertisementType_ConnectableUndirected) = 0, ConnectableDirected (BluetoothLEAdvertisementType_ConnectableDirected) = 1, ScannableUndirected (BluetoothLEAdvertisementType_ScannableUndirected) = 2, NonConnectableUndirected (BluetoothLEAdvertisementType_NonConnectableUndirected) = 3, ScanResponse (BluetoothLEAdvertisementType_ScanResponse) = 4,
		}}
		RT_ENUM! { enum BluetoothLEAdvertisementWatcherStatus: i32 {
			Created (BluetoothLEAdvertisementWatcherStatus_Created) = 0, Started (BluetoothLEAdvertisementWatcherStatus_Started) = 1, Stopping (BluetoothLEAdvertisementWatcherStatus_Stopping) = 2, Stopped (BluetoothLEAdvertisementWatcherStatus_Stopped) = 3, Aborted (BluetoothLEAdvertisementWatcherStatus_Aborted) = 4,
		}}
		DEFINE_IID!(IID_IBluetoothLEAdvertisement, 107983543, 13265, 20093, 131, 103, 207, 129, 208, 247, 150, 83);
		RT_INTERFACE!{interface IBluetoothLEAdvertisement(IBluetoothLEAdvertisementVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisement] {
			fn get_Flags(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementFlags>) -> ::w::HRESULT,
			fn put_Flags(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementFlags>) -> ::w::HRESULT,
			fn get_LocalName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_LocalName(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_ServiceUuids(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<::Guid>) -> ::w::HRESULT,
			fn get_ManufacturerData(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEManufacturerData>) -> ::w::HRESULT,
			fn get_DataSections(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementDataSection>) -> ::w::HRESULT,
			fn GetManufacturerDataByCompanyId(&mut self, companyId: u16, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEManufacturerData>) -> ::w::HRESULT,
			fn GetSectionsByType(&mut self, type_: u8, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementDataSection>) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEManufacturerData: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEManufacturerData);
		RT_CLASS!(BluetoothLEAdvertisementDataSection: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisementDataSection);
		RT_CLASS!(BluetoothLEAdvertisement: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisement);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementFilter, 320778451, 53326, 18353, 131, 126, 73, 64, 91, 246, 248, 15);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementFilter(IBluetoothLEAdvertisementFilterVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementFilter] {
			fn get_Advertisement(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisement) -> ::w::HRESULT,
			fn put_Advertisement(&mut self, value: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisement) -> ::w::HRESULT,
			fn get_BytePatterns(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementBytePattern>) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAdvertisementBytePattern: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisementBytePattern);
		RT_CLASS!(BluetoothLEAdvertisementFilter: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisementFilter);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementWatcherStoppedEventArgs, 3712022605, 59321, 17379, 156, 4, 6, 133, 208, 133, 253, 140);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementWatcherStoppedEventArgs(IBluetoothLEAdvertisementWatcherStoppedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementWatcherStoppedEventArgs] {
			fn get_Error(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothError) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAdvertisementWatcherStoppedEventArgs: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisementWatcherStoppedEventArgs);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementWatcherFactory, 2595171670, 14764, 17726, 179, 42, 133, 198, 87, 224, 23, 241);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementWatcherFactory(IBluetoothLEAdvertisementWatcherFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementWatcherFactory] {
			fn Create(&mut self, advertisementFilter: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementFilter, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementWatcher) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAdvertisementWatcher: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisementWatcher);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementWatcher, 2796303215, 62419, 17047, 141, 108, 200, 30, 166, 98, 63, 64);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementWatcher(IBluetoothLEAdvertisementWatcherVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementWatcher] {
			fn get_MinSamplingInterval(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_MaxSamplingInterval(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_MinOutOfRangeTimeout(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_MaxOutOfRangeTimeout(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementWatcherStatus) -> ::w::HRESULT,
			fn get_ScanningMode(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEScanningMode) -> ::w::HRESULT,
			fn put_ScanningMode(&mut self, value: ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEScanningMode) -> ::w::HRESULT,
			fn get_SignalStrengthFilter(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothSignalStrengthFilter) -> ::w::HRESULT,
			fn put_SignalStrengthFilter(&mut self, value: *mut ::rt::gen::windows::devices::bluetooth::BluetoothSignalStrengthFilter) -> ::w::HRESULT,
			fn get_AdvertisementFilter(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementFilter) -> ::w::HRESULT,
			fn put_AdvertisementFilter(&mut self, value: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementFilter) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT,
			fn add_Received(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementWatcher, &::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Received(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Stopped(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementWatcher, &::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementWatcherStoppedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Stopped(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAdvertisementReceivedEventArgs: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisementReceivedEventArgs);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementReceivedEventArgs, 664305119, 58774, 16830, 141, 67, 158, 103, 49, 212, 169, 19);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementReceivedEventArgs(IBluetoothLEAdvertisementReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementReceivedEventArgs] {
			fn get_RawSignalStrengthInDBm(&mut self, out: *mut i16) -> ::w::HRESULT,
			fn get_BluetoothAddress(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn get_AdvertisementType(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementType) -> ::w::HRESULT,
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_Advertisement(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisement) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAdvertisementDataSectionFactory, 3886287170, 43077, 16453, 191, 126, 62, 153, 113, 219, 138, 107);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementDataSectionFactory(IBluetoothLEAdvertisementDataSectionFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementDataSectionFactory] {
			fn Create(&mut self, dataType: u8, data: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementDataSection) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAdvertisementDataSection, 3609277204, 14915, 16633, 182, 240, 146, 191, 239, 195, 74, 227);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementDataSection(IBluetoothLEAdvertisementDataSectionVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementDataSection] {
			fn get_DataType(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn put_DataType(&mut self, value: u8) -> ::w::HRESULT,
			fn get_Data(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_Data(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEManufacturerDataFactory, 3231398392, 12698, 17438, 141, 229, 102, 168, 30, 135, 122, 108);
		RT_INTERFACE!{interface IBluetoothLEManufacturerDataFactory(IBluetoothLEManufacturerDataFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEManufacturerDataFactory] {
			fn Create(&mut self, companyId: u16, data: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEManufacturerData) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEManufacturerData, 2435693080, 26979, 17715, 176, 97, 70, 148, 218, 251, 52, 229);
		RT_INTERFACE!{interface IBluetoothLEManufacturerData(IBluetoothLEManufacturerDataVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEManufacturerData] {
			fn get_CompanyId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn put_CompanyId(&mut self, value: u16) -> ::w::HRESULT,
			fn get_Data(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_Data(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAdvertisementBytePatternFactory, 3269610867, 64860, 20163, 190, 42, 156, 166, 250, 17, 183, 189);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementBytePatternFactory(IBluetoothLEAdvertisementBytePatternFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementBytePatternFactory] {
			fn Create(&mut self, dataType: u8, offset: i16, data: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementBytePattern) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAdvertisementBytePattern, 4227520498, 47557, 18952, 188, 81, 80, 47, 142, 246, 138, 121);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementBytePattern(IBluetoothLEAdvertisementBytePatternVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementBytePattern] {
			fn get_DataType(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn put_DataType(&mut self, value: u8) -> ::w::HRESULT,
			fn get_Offset(&mut self, out: *mut i16) -> ::w::HRESULT,
			fn put_Offset(&mut self, value: i16) -> ::w::HRESULT,
			fn get_Data(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_Data(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBluetoothLEAdvertisementDataTypesStatics, 1001801519, 1542, 17227, 167, 110, 116, 21, 159, 6, 132, 211);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementDataTypesStatics(IBluetoothLEAdvertisementDataTypesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementDataTypesStatics] {
			fn get_Flags(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_IncompleteService16BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_CompleteService16BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_IncompleteService32BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_CompleteService32BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_IncompleteService128BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_CompleteService128BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ShortenedLocalName(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_CompleteLocalName(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_TxPowerLevel(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SlaveConnectionIntervalRange(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ServiceSolicitation16BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ServiceSolicitation32BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ServiceSolicitation128BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ServiceData16BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ServiceData32BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ServiceData128BitUuids(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_PublicTargetAddress(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_RandomTargetAddress(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Appearance(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_AdvertisingInterval(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ManufacturerSpecificData(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		RT_ENUM! { enum BluetoothLEAdvertisementPublisherStatus: i32 {
			Created (BluetoothLEAdvertisementPublisherStatus_Created) = 0, Waiting (BluetoothLEAdvertisementPublisherStatus_Waiting) = 1, Started (BluetoothLEAdvertisementPublisherStatus_Started) = 2, Stopping (BluetoothLEAdvertisementPublisherStatus_Stopping) = 3, Stopped (BluetoothLEAdvertisementPublisherStatus_Stopped) = 4, Aborted (BluetoothLEAdvertisementPublisherStatus_Aborted) = 5,
		}}
		DEFINE_IID!(IID_IBluetoothLEAdvertisementPublisherStatusChangedEventArgs, 163757471, 11775, 19235, 134, 238, 13, 20, 251, 148, 174, 174);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementPublisherStatusChangedEventArgs(IBluetoothLEAdvertisementPublisherStatusChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementPublisherStatusChangedEventArgs] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementPublisherStatus) -> ::w::HRESULT,
			fn get_Error(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothError) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAdvertisementPublisherStatusChangedEventArgs: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisementPublisherStatusChangedEventArgs);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementPublisherFactory, 1549731422, 47203, 18817, 161, 175, 28, 84, 77, 139, 12, 13);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementPublisherFactory(IBluetoothLEAdvertisementPublisherFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementPublisherFactory] {
			fn Create(&mut self, advertisement: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisement, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementPublisher) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAdvertisementPublisher: ::rt::gen::windows::devices::bluetooth::advertisement::IBluetoothLEAdvertisementPublisher);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementPublisher, 3454542073, 55802, 17366, 162, 100, 221, 216, 183, 218, 139, 120);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementPublisher(IBluetoothLEAdvertisementPublisherVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementPublisher] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementPublisherStatus) -> ::w::HRESULT,
			fn get_Advertisement(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisement) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT,
			fn add_StatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementPublisher, &::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementPublisherStatusChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
} // Windows.Devices.Bluetooth.Advertisement
pub mod background { // Windows.Devices.Bluetooth.Background
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(RfcommInboundConnectionInformation: ::rt::gen::windows::devices::bluetooth::background::IRfcommInboundConnectionInformation);
		RT_CLASS!(RfcommOutboundConnectionInformation: ::rt::gen::windows::devices::bluetooth::background::IRfcommOutboundConnectionInformation);
		DEFINE_IID!(IID_IRfcommInboundConnectionInformation, 1832809896, 21545, 16473, 146, 227, 30, 139, 101, 82, 135, 7);
		RT_INTERFACE!{interface IRfcommInboundConnectionInformation(IRfcommInboundConnectionInformationVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommInboundConnectionInformation] {
			fn get_SdpRecord(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_SdpRecord(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_LocalServiceId(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn put_LocalServiceId(&mut self, value: *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn get_ServiceCapabilities(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothServiceCapabilities) -> ::w::HRESULT,
			fn put_ServiceCapabilities(&mut self, value: ::rt::gen::windows::devices::bluetooth::BluetoothServiceCapabilities) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IRfcommOutboundConnectionInformation, 2962301563, 62516, 19632, 153, 177, 74, 184, 206, 218, 237, 215);
		RT_INTERFACE!{interface IRfcommOutboundConnectionInformation(IRfcommOutboundConnectionInformationVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommOutboundConnectionInformation] {
			fn get_RemoteServiceId(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT,
			fn put_RemoteServiceId(&mut self, value: *mut ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceId) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IRfcommConnectionTriggerDetails, 4179784525, 11836, 20220, 171, 89, 252, 92, 249, 111, 151, 227);
		RT_INTERFACE!{interface IRfcommConnectionTriggerDetails(IRfcommConnectionTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IRfcommConnectionTriggerDetails] {
			fn get_Socket(&mut self, out: *mut *mut ::rt::gen::windows::networking::sockets::StreamSocket) -> ::w::HRESULT,
			fn get_Incoming(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_RemoteDevice(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothDevice) -> ::w::HRESULT
		}}
		RT_CLASS!(RfcommConnectionTriggerDetails: ::rt::gen::windows::devices::bluetooth::background::IRfcommConnectionTriggerDetails);
		DEFINE_IID!(IID_IGattCharacteristicNotificationTriggerDetails, 2610969368, 4076, 17258, 147, 177, 244, 108, 105, 117, 50, 162);
		RT_INTERFACE!{interface IGattCharacteristicNotificationTriggerDetails(IGattCharacteristicNotificationTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IGattCharacteristicNotificationTriggerDetails] {
			fn get_Characteristic(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic) -> ::w::HRESULT,
			fn get_Value(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		RT_CLASS!(GattCharacteristicNotificationTriggerDetails: ::rt::gen::windows::devices::bluetooth::background::IGattCharacteristicNotificationTriggerDetails);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementWatcherTriggerDetails, 2816170711, 8791, 20073, 151, 132, 254, 230, 69, 193, 220, 224);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementWatcherTriggerDetails(IBluetoothLEAdvertisementWatcherTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementWatcherTriggerDetails] {
			fn get_Error(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothError) -> ::w::HRESULT,
			fn get_Advertisements(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementReceivedEventArgs>) -> ::w::HRESULT,
			fn get_SignalStrengthFilter(&mut self, out: *mut *mut ::rt::gen::windows::devices::bluetooth::BluetoothSignalStrengthFilter) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAdvertisementWatcherTriggerDetails: ::rt::gen::windows::devices::bluetooth::background::IBluetoothLEAdvertisementWatcherTriggerDetails);
		DEFINE_IID!(IID_IBluetoothLEAdvertisementPublisherTriggerDetails, 1628359302, 13440, 16841, 169, 24, 125, 218, 223, 32, 126, 0);
		RT_INTERFACE!{interface IBluetoothLEAdvertisementPublisherTriggerDetails(IBluetoothLEAdvertisementPublisherTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IBluetoothLEAdvertisementPublisherTriggerDetails] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementPublisherStatus) -> ::w::HRESULT,
			fn get_Error(&mut self, out: *mut ::rt::gen::windows::devices::bluetooth::BluetoothError) -> ::w::HRESULT
		}}
		RT_CLASS!(BluetoothLEAdvertisementPublisherTriggerDetails: ::rt::gen::windows::devices::bluetooth::background::IBluetoothLEAdvertisementPublisherTriggerDetails);
} // Windows.Devices.Bluetooth.Background
} // Windows.Devices.Bluetooth
pub mod enumeration { // Windows.Devices.Enumeration
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_IDeviceConnectionChangeTriggerDetails, 3092745228, 48065, 18507, 191, 250, 123, 49, 220, 194, 0, 178);
		RT_INTERFACE!{interface IDeviceConnectionChangeTriggerDetails(IDeviceConnectionChangeTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceConnectionChangeTriggerDetails] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceConnectionChangeTriggerDetails: ::rt::gen::windows::devices::enumeration::IDeviceConnectionChangeTriggerDetails);
		RT_ENUM! { enum DevicePickerDisplayStatusOptions: u32 {
			None (DevicePickerDisplayStatusOptions_None) = 0, ShowProgress (DevicePickerDisplayStatusOptions_ShowProgress) = 1, ShowDisconnectButton (DevicePickerDisplayStatusOptions_ShowDisconnectButton) = 2, ShowRetryButton (DevicePickerDisplayStatusOptions_ShowRetryButton) = 4,
		}}
		DEFINE_IID!(IID_IDevicePickerAppearance, 3868857030, 58919, 20184, 155, 108, 70, 10, 244, 69, 229, 109);
		RT_INTERFACE!{interface IDevicePickerAppearance(IDevicePickerAppearanceVtbl): IInspectable(IInspectableVtbl) [IID_IDevicePickerAppearance] {
			fn get_Title(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_Title(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_ForegroundColor(&mut self, out: *mut ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn put_ForegroundColor(&mut self, value: ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn get_BackgroundColor(&mut self, out: *mut ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn put_BackgroundColor(&mut self, value: ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn get_AccentColor(&mut self, out: *mut ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn put_AccentColor(&mut self, value: ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn get_SelectedForegroundColor(&mut self, out: *mut ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn put_SelectedForegroundColor(&mut self, value: ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn get_SelectedBackgroundColor(&mut self, out: *mut ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn put_SelectedBackgroundColor(&mut self, value: ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn get_SelectedAccentColor(&mut self, out: *mut ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn put_SelectedAccentColor(&mut self, value: ::rt::gen::windows::ui::Color) -> ::w::HRESULT
		}}
		RT_CLASS!(DevicePickerAppearance: ::rt::gen::windows::devices::enumeration::IDevicePickerAppearance);
		DEFINE_IID!(IID_IDeviceSelectedEventArgs, 647944926, 7471, 18752, 132, 2, 65, 86, 184, 29, 60, 119);
		RT_INTERFACE!{interface IDeviceSelectedEventArgs(IDeviceSelectedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceSelectedEventArgs] {
			fn get_SelectedDevice(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceSelectedEventArgs: ::rt::gen::windows::devices::enumeration::IDeviceSelectedEventArgs);
		DEFINE_IID!(IID_IDeviceDisconnectButtonClickedEventArgs, 2386867565, 63746, 18944, 181, 54, 243, 121, 146, 230, 162, 167);
		RT_INTERFACE!{interface IDeviceDisconnectButtonClickedEventArgs(IDeviceDisconnectButtonClickedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceDisconnectButtonClickedEventArgs] {
			fn get_Device(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceDisconnectButtonClickedEventArgs: ::rt::gen::windows::devices::enumeration::IDeviceDisconnectButtonClickedEventArgs);
		DEFINE_IID!(IID_IDevicePickerFilter, 2447086242, 22475, 18673, 155, 89, 165, 155, 122, 31, 2, 162);
		RT_INTERFACE!{interface IDevicePickerFilter(IDevicePickerFilterVtbl): IInspectable(IInspectableVtbl) [IID_IDevicePickerFilter] {
			fn get_SupportedDeviceClasses(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<::rt::gen::windows::devices::enumeration::DeviceClass>) -> ::w::HRESULT,
			fn get_SupportedDeviceSelectors(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT
		}}
		RT_CLASS!(DevicePickerFilter: ::rt::gen::windows::devices::enumeration::IDevicePickerFilter);
		DEFINE_IID!(IID_IDevicePicker, 2224650914, 842, 17472, 136, 19, 125, 11, 212, 121, 191, 90);
		RT_INTERFACE!{interface IDevicePicker(IDevicePickerVtbl): IInspectable(IInspectableVtbl) [IID_IDevicePicker] {
			fn get_Filter(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DevicePickerFilter) -> ::w::HRESULT,
			fn get_Appearance(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DevicePickerAppearance) -> ::w::HRESULT,
			fn get_RequestedProperties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn add_DeviceSelected(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DevicePicker, &::rt::gen::windows::devices::enumeration::DeviceSelectedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_DeviceSelected(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_DisconnectButtonClicked(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DevicePicker, &::rt::gen::windows::devices::enumeration::DeviceDisconnectButtonClickedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_DisconnectButtonClicked(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_DevicePickerDismissed(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DevicePicker, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_DevicePickerDismissed(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn Show(&mut self, selection: ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn ShowWithPlacement(&mut self, selection: ::rt::gen::windows::foundation::Rect, placement: ::rt::gen::windows::ui::popups::Placement) -> ::w::HRESULT,
			fn PickSingleDeviceAsync(&mut self, selection: ::rt::gen::windows::foundation::Rect, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformation>) -> ::w::HRESULT,
			fn PickSingleDeviceAsyncWithPlacement(&mut self, selection: ::rt::gen::windows::foundation::Rect, placement: ::rt::gen::windows::ui::popups::Placement, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformation>) -> ::w::HRESULT,
			fn Hide(&mut self) -> ::w::HRESULT,
			fn SetDisplayStatus(&mut self, device: *mut ::rt::gen::windows::devices::enumeration::DeviceInformation, status: ::w::HSTRING, options: ::rt::gen::windows::devices::enumeration::DevicePickerDisplayStatusOptions) -> ::w::HRESULT
		}}
		RT_CLASS!(DevicePicker: ::rt::gen::windows::devices::enumeration::IDevicePicker);
		RT_ENUM! { enum DeviceClass: i32 {
			All (DeviceClass_All) = 0, AudioCapture (DeviceClass_AudioCapture) = 1, AudioRender (DeviceClass_AudioRender) = 2, PortableStorageDevice (DeviceClass_PortableStorageDevice) = 3, VideoCapture (DeviceClass_VideoCapture) = 4, ImageScanner (DeviceClass_ImageScanner) = 5, Location (DeviceClass_Location) = 6,
		}}
		RT_ENUM! { enum DeviceWatcherStatus: i32 {
			Created (DeviceWatcherStatus_Created) = 0, Started (DeviceWatcherStatus_Started) = 1, EnumerationCompleted (DeviceWatcherStatus_EnumerationCompleted) = 2, Stopping (DeviceWatcherStatus_Stopping) = 3, Stopped (DeviceWatcherStatus_Stopped) = 4, Aborted (DeviceWatcherStatus_Aborted) = 5,
		}}
		RT_CLASS!(DeviceThumbnail: ::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType);
		RT_ENUM! { enum Panel: i32 {
			Unknown (Panel_Unknown) = 0, Front (Panel_Front) = 1, Back (Panel_Back) = 2, Top (Panel_Top) = 3, Bottom (Panel_Bottom) = 4, Left (Panel_Left) = 5, Right (Panel_Right) = 6,
		}}
		DEFINE_IID!(IID_IEnclosureLocation, 1110706727, 22544, 17820, 170, 187, 198, 94, 31, 129, 62, 207);
		RT_INTERFACE!{interface IEnclosureLocation(IEnclosureLocationVtbl): IInspectable(IInspectableVtbl) [IID_IEnclosureLocation] {
			fn get_InDock(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_InLid(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Panel(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::Panel) -> ::w::HRESULT
		}}
		RT_CLASS!(EnclosureLocation: ::rt::gen::windows::devices::enumeration::IEnclosureLocation);
		RT_ENUM! { enum DeviceInformationKind: i32 {
			Unknown (DeviceInformationKind_Unknown) = 0, DeviceInterface (DeviceInformationKind_DeviceInterface) = 1, DeviceContainer (DeviceInformationKind_DeviceContainer) = 2, Device (DeviceInformationKind_Device) = 3, DeviceInterfaceClass (DeviceInformationKind_DeviceInterfaceClass) = 4, AssociationEndpoint (DeviceInformationKind_AssociationEndpoint) = 5, AssociationEndpointContainer (DeviceInformationKind_AssociationEndpointContainer) = 6, AssociationEndpointService (DeviceInformationKind_AssociationEndpointService) = 7,
		}}
		DEFINE_IID!(IID_IDeviceInformationUpdate, 2402374405, 55666, 17591, 163, 126, 158, 130, 44, 120, 33, 59);
		RT_INTERFACE!{interface IDeviceInformationUpdate(IDeviceInformationUpdateVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformationUpdate] {
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeviceInformationUpdate2, 1570575500, 43123, 18526, 186, 166, 170, 98, 7, 136, 227, 204);
		RT_INTERFACE!{interface IDeviceInformationUpdate2(IDeviceInformationUpdate2Vtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformationUpdate2] {
			fn get_Kind(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceInformationKind) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceInformationUpdate: ::rt::gen::windows::devices::enumeration::IDeviceInformationUpdate);
		RT_CLASS!(DeviceInformationCollection: ::rt::gen::windows::foundation::collections::IVectorView<&'static ::rt::gen::windows::devices::enumeration::DeviceInformation>);
		DEFINE_IID!(IID_IDeviceWatcher, 3387603325, 36715, 20374, 169, 244, 171, 200, 20, 226, 34, 113);
		RT_INTERFACE!{interface IDeviceWatcher(IDeviceWatcherVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceWatcher] {
			fn add_Added(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DeviceWatcher, &::rt::gen::windows::devices::enumeration::DeviceInformation>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Added(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Updated(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DeviceWatcher, &::rt::gen::windows::devices::enumeration::DeviceInformationUpdate>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Updated(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Removed(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DeviceWatcher, &::rt::gen::windows::devices::enumeration::DeviceInformationUpdate>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Removed(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_EnumerationCompleted(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DeviceWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_EnumerationCompleted(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Stopped(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DeviceWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Stopped(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceWatcherStatus) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceWatcher: ::rt::gen::windows::devices::enumeration::IDeviceWatcher);
		RT_CLASS!(DeviceInformation: ::rt::gen::windows::devices::enumeration::IDeviceInformation);
		RT_ENUM! { enum DeviceWatcherEventKind: i32 {
			Add (DeviceWatcherEventKind_Add) = 0, Update (DeviceWatcherEventKind_Update) = 1, Remove (DeviceWatcherEventKind_Remove) = 2,
		}}
		DEFINE_IID!(IID_IDeviceWatcher2, 4278732142, 60692, 18921, 154, 105, 129, 23, 197, 74, 233, 113);
		RT_INTERFACE!{interface IDeviceWatcher2(IDeviceWatcher2Vtbl): IInspectable(IInspectableVtbl) [IID_IDeviceWatcher2] {
			fn GetBackgroundTrigger(&mut self, requestedEventKinds: *mut ::rt::gen::windows::foundation::collections::IIterable<::rt::gen::windows::devices::enumeration::DeviceWatcherEventKind>, out: *mut *mut ::rt::gen::windows::applicationmodel::background::DeviceWatcherTrigger) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeviceInformationStatics, 3246329870, 14918, 19064, 128, 19, 118, 157, 201, 185, 115, 144);
		RT_INTERFACE!{interface IDeviceInformationStatics(IDeviceInformationStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformationStatics] {
			fn CreateFromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformation>) -> ::w::HRESULT,
			fn CreateFromIdAsyncAdditionalProperties(&mut self, deviceId: ::w::HSTRING, additionalProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformation>) -> ::w::HRESULT,
			fn FindAllAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformationCollection>) -> ::w::HRESULT,
			fn FindAllAsyncDeviceClass(&mut self, deviceClass: ::rt::gen::windows::devices::enumeration::DeviceClass, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformationCollection>) -> ::w::HRESULT,
			fn FindAllAsyncAqsFilter(&mut self, aqsFilter: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformationCollection>) -> ::w::HRESULT,
			fn FindAllAsyncAqsFilterAndAdditionalProperties(&mut self, aqsFilter: ::w::HSTRING, additionalProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformationCollection>) -> ::w::HRESULT,
			fn CreateWatcher(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceWatcher) -> ::w::HRESULT,
			fn CreateWatcherDeviceClass(&mut self, deviceClass: ::rt::gen::windows::devices::enumeration::DeviceClass, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceWatcher) -> ::w::HRESULT,
			fn CreateWatcherAqsFilter(&mut self, aqsFilter: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceWatcher) -> ::w::HRESULT,
			fn CreateWatcherAqsFilterAndAdditionalProperties(&mut self, aqsFilter: ::w::HSTRING, additionalProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceWatcher) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeviceInformationStatics2, 1228623668, 43087, 17917, 145, 103, 21, 209, 203, 27, 209, 249);
		RT_INTERFACE!{interface IDeviceInformationStatics2(IDeviceInformationStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformationStatics2] {
			fn GetAqsFilterFromDeviceClass(&mut self, deviceClass: ::rt::gen::windows::devices::enumeration::DeviceClass, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn CreateFromIdAsyncWithKindAndAdditionalProperties(&mut self, deviceId: ::w::HSTRING, additionalProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, kind: ::rt::gen::windows::devices::enumeration::DeviceInformationKind, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformation>) -> ::w::HRESULT,
			fn FindAllAsyncWithKindAqsFilterAndAdditionalProperties(&mut self, aqsFilter: ::w::HSTRING, additionalProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, kind: ::rt::gen::windows::devices::enumeration::DeviceInformationKind, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceInformationCollection>) -> ::w::HRESULT,
			fn CreateWatcherWithKindAqsFilterAndAdditionalProperties(&mut self, aqsFilter: ::w::HSTRING, additionalProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, kind: ::rt::gen::windows::devices::enumeration::DeviceInformationKind, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceWatcher) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeviceInformation, 2879454101, 17304, 18589, 142, 68, 230, 19, 9, 39, 1, 31);
		RT_INTERFACE!{interface IDeviceInformation(IDeviceInformationVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformation] {
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDefault(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_EnclosureLocation(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::EnclosureLocation) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT,
			fn Update(&mut self, updateInfo: *mut ::rt::gen::windows::devices::enumeration::DeviceInformationUpdate) -> ::w::HRESULT,
			fn GetThumbnailAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceThumbnail>) -> ::w::HRESULT,
			fn GetGlyphThumbnailAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceThumbnail>) -> ::w::HRESULT
		}}
		RT_ENUM! { enum DevicePairingKinds: u32 {
			None (DevicePairingKinds_None) = 0, ConfirmOnly (DevicePairingKinds_ConfirmOnly) = 1, DisplayPin (DevicePairingKinds_DisplayPin) = 2, ProvidePin (DevicePairingKinds_ProvidePin) = 4, ConfirmPinMatch (DevicePairingKinds_ConfirmPinMatch) = 8,
		}}
		RT_ENUM! { enum DevicePairingResultStatus: i32 {
			Paired (DevicePairingResultStatus_Paired) = 0, NotReadyToPair (DevicePairingResultStatus_NotReadyToPair) = 1, NotPaired (DevicePairingResultStatus_NotPaired) = 2, AlreadyPaired (DevicePairingResultStatus_AlreadyPaired) = 3, ConnectionRejected (DevicePairingResultStatus_ConnectionRejected) = 4, TooManyConnections (DevicePairingResultStatus_TooManyConnections) = 5, HardwareFailure (DevicePairingResultStatus_HardwareFailure) = 6, AuthenticationTimeout (DevicePairingResultStatus_AuthenticationTimeout) = 7, AuthenticationNotAllowed (DevicePairingResultStatus_AuthenticationNotAllowed) = 8, AuthenticationFailure (DevicePairingResultStatus_AuthenticationFailure) = 9, NoSupportedProfiles (DevicePairingResultStatus_NoSupportedProfiles) = 10, ProtectionLevelCouldNotBeMet (DevicePairingResultStatus_ProtectionLevelCouldNotBeMet) = 11, AccessDenied (DevicePairingResultStatus_AccessDenied) = 12, InvalidCeremonyData (DevicePairingResultStatus_InvalidCeremonyData) = 13, PairingCanceled (DevicePairingResultStatus_PairingCanceled) = 14, OperationAlreadyInProgress (DevicePairingResultStatus_OperationAlreadyInProgress) = 15, RequiredHandlerNotRegistered (DevicePairingResultStatus_RequiredHandlerNotRegistered) = 16, RejectedByHandler (DevicePairingResultStatus_RejectedByHandler) = 17, RemoteDeviceHasAssociation (DevicePairingResultStatus_RemoteDeviceHasAssociation) = 18, Failed (DevicePairingResultStatus_Failed) = 19,
		}}
		RT_ENUM! { enum DeviceUnpairingResultStatus: i32 {
			Unpaired (DeviceUnpairingResultStatus_Unpaired) = 0, AlreadyUnpaired (DeviceUnpairingResultStatus_AlreadyUnpaired) = 1, OperationAlreadyInProgress (DeviceUnpairingResultStatus_OperationAlreadyInProgress) = 2, AccessDenied (DeviceUnpairingResultStatus_AccessDenied) = 3, Failed (DeviceUnpairingResultStatus_Failed) = 4,
		}}
		RT_ENUM! { enum DevicePairingProtectionLevel: i32 {
			Default (DevicePairingProtectionLevel_Default) = 0, None (DevicePairingProtectionLevel_None) = 1, Encryption (DevicePairingProtectionLevel_Encryption) = 2, EncryptionAndAuthentication (DevicePairingProtectionLevel_EncryptionAndAuthentication) = 3,
		}}
		DEFINE_IID!(IID_IDevicePairingResult, 120259263, 56725, 16421, 155, 55, 222, 81, 173, 186, 55, 183);
		RT_INTERFACE!{interface IDevicePairingResult(IDevicePairingResultVtbl): IInspectable(IInspectableVtbl) [IID_IDevicePairingResult] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DevicePairingResultStatus) -> ::w::HRESULT,
			fn get_ProtectionLevelUsed(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DevicePairingProtectionLevel) -> ::w::HRESULT
		}}
		RT_CLASS!(DevicePairingResult: ::rt::gen::windows::devices::enumeration::IDevicePairingResult);
		DEFINE_IID!(IID_IDeviceUnpairingResult, 1727285971, 31193, 17483, 146, 207, 169, 46, 247, 37, 113, 199);
		RT_INTERFACE!{interface IDeviceUnpairingResult(IDeviceUnpairingResultVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceUnpairingResult] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceUnpairingResultStatus) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceUnpairingResult: ::rt::gen::windows::devices::enumeration::IDeviceUnpairingResult);
		DEFINE_IID!(IID_IDevicePairingSettings, 1210888828, 33723, 16910, 190, 81, 102, 2, 178, 34, 222, 84);
		RT_INTERFACE!{interface IDevicePairingSettings(IDevicePairingSettingsVtbl): IInspectable(IInspectableVtbl) [IID_IDevicePairingSettings] {
			
		}}
		DEFINE_IID!(IID_IDevicePairingRequestedEventArgs, 4145544278, 56939, 18559, 131, 118, 1, 128, 172, 166, 153, 99);
		RT_INTERFACE!{interface IDevicePairingRequestedEventArgs(IDevicePairingRequestedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IDevicePairingRequestedEventArgs] {
			fn get_DeviceInformation(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT,
			fn get_PairingKind(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DevicePairingKinds) -> ::w::HRESULT,
			fn get_Pin(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn Accept(&mut self) -> ::w::HRESULT,
			fn AcceptWithPin(&mut self, pin: ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeferral(&mut self, out: *mut *mut ::rt::gen::windows::foundation::Deferral) -> ::w::HRESULT
		}}
		RT_CLASS!(DevicePairingRequestedEventArgs: ::rt::gen::windows::devices::enumeration::IDevicePairingRequestedEventArgs);
		DEFINE_IID!(IID_IDeviceInformationCustomPairing, 2232650754, 20198, 18708, 131, 112, 16, 122, 57, 20, 76, 14);
		RT_INTERFACE!{interface IDeviceInformationCustomPairing(IDeviceInformationCustomPairingVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformationCustomPairing] {
			fn PairAsync(&mut self, pairingKindsSupported: ::rt::gen::windows::devices::enumeration::DevicePairingKinds, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DevicePairingResult>) -> ::w::HRESULT,
			fn PairWithProtectionLevelAsync(&mut self, pairingKindsSupported: ::rt::gen::windows::devices::enumeration::DevicePairingKinds, minProtectionLevel: ::rt::gen::windows::devices::enumeration::DevicePairingProtectionLevel, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DevicePairingResult>) -> ::w::HRESULT,
			fn PairWithProtectionLevelAndSettingsAsync(&mut self, pairingKindsSupported: ::rt::gen::windows::devices::enumeration::DevicePairingKinds, minProtectionLevel: ::rt::gen::windows::devices::enumeration::DevicePairingProtectionLevel, devicePairingSettings: *mut ::rt::gen::windows::devices::enumeration::IDevicePairingSettings, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DevicePairingResult>) -> ::w::HRESULT,
			fn add_PairingRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DeviceInformationCustomPairing, &::rt::gen::windows::devices::enumeration::DevicePairingRequestedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_PairingRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceInformationCustomPairing: ::rt::gen::windows::devices::enumeration::IDeviceInformationCustomPairing);
		DEFINE_IID!(IID_IDeviceInformationPairing, 742877685, 63108, 16597, 132, 105, 232, 219, 170, 183, 4, 133);
		RT_INTERFACE!{interface IDeviceInformationPairing(IDeviceInformationPairingVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformationPairing] {
			fn get_IsPaired(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_CanPair(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn PairAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DevicePairingResult>) -> ::w::HRESULT,
			fn PairWithProtectionLevelAsync(&mut self, minProtectionLevel: ::rt::gen::windows::devices::enumeration::DevicePairingProtectionLevel, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DevicePairingResult>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeviceInformationPairing2, 4135981821, 2798, 17192, 133, 204, 28, 116, 43, 177, 121, 13);
		RT_INTERFACE!{interface IDeviceInformationPairing2(IDeviceInformationPairing2Vtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformationPairing2] {
			fn get_ProtectionLevel(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DevicePairingProtectionLevel) -> ::w::HRESULT,
			fn get_Custom(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformationCustomPairing) -> ::w::HRESULT,
			fn PairWithProtectionLevelAndSettingsAsync(&mut self, minProtectionLevel: ::rt::gen::windows::devices::enumeration::DevicePairingProtectionLevel, devicePairingSettings: *mut ::rt::gen::windows::devices::enumeration::IDevicePairingSettings, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DevicePairingResult>) -> ::w::HRESULT,
			fn UnpairAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::DeviceUnpairingResult>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeviceInformationPairingStatics, 3910517768, 14036, 18849, 191, 19, 81, 65, 115, 121, 155, 107);
		RT_INTERFACE!{interface IDeviceInformationPairingStatics(IDeviceInformationPairingStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformationPairingStatics] {
			fn TryRegisterForAllInboundPairingRequests(&mut self, pairingKindsSupported: ::rt::gen::windows::devices::enumeration::DevicePairingKinds, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceInformationPairing: ::rt::gen::windows::devices::enumeration::IDeviceInformationPairing);
		DEFINE_IID!(IID_IDeviceInformation2, 4048987704, 31127, 18649, 161, 12, 38, 157, 70, 83, 63, 72);
		RT_INTERFACE!{interface IDeviceInformation2(IDeviceInformation2Vtbl): IInspectable(IInspectableVtbl) [IID_IDeviceInformation2] {
			fn get_Kind(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceInformationKind) -> ::w::HRESULT,
			fn get_Pairing(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformationPairing) -> ::w::HRESULT
		}}
		RT_ENUM! { enum DeviceAccessStatus: i32 {
			Unspecified (DeviceAccessStatus_Unspecified) = 0, Allowed (DeviceAccessStatus_Allowed) = 1, DeniedByUser (DeviceAccessStatus_DeniedByUser) = 2, DeniedBySystem (DeviceAccessStatus_DeniedBySystem) = 3,
		}}
		DEFINE_IID!(IID_IDeviceAccessChangedEventArgs, 3738831820, 20381, 20312, 157, 186, 169, 188, 128, 4, 8, 213);
		RT_INTERFACE!{interface IDeviceAccessChangedEventArgs(IDeviceAccessChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceAccessChangedEventArgs] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceAccessStatus) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceAccessChangedEventArgs: ::rt::gen::windows::devices::enumeration::IDeviceAccessChangedEventArgs);
		DEFINE_IID!(IID_IDeviceAccessInformation, 195730035, 28133, 18709, 141, 221, 154, 5, 84, 166, 245, 69);
		RT_INTERFACE!{interface IDeviceAccessInformation(IDeviceAccessInformationVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceAccessInformation] {
			fn add_AccessChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::DeviceAccessInformation, &::rt::gen::windows::devices::enumeration::DeviceAccessChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AccessChanged(&mut self, cookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_CurrentStatus(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceAccessStatus) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceAccessInformation: ::rt::gen::windows::devices::enumeration::IDeviceAccessInformation);
		DEFINE_IID!(IID_IDeviceAccessInformationStatics, 1464587219, 24368, 17869, 138, 148, 114, 79, 229, 151, 48, 132);
		RT_INTERFACE!{interface IDeviceAccessInformationStatics(IDeviceAccessInformationStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceAccessInformationStatics] {
			fn CreateFromId(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceAccessInformation) -> ::w::HRESULT,
			fn CreateFromDeviceClassId(&mut self, deviceClassId: ::w::GUID, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceAccessInformation) -> ::w::HRESULT,
			fn CreateFromDeviceClass(&mut self, deviceClass: ::rt::gen::windows::devices::enumeration::DeviceClass, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceAccessInformation) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeviceWatcherEvent, 1957338123, 7613, 18429, 182, 53, 60, 197, 86, 208, 255, 139);
		RT_INTERFACE!{interface IDeviceWatcherEvent(IDeviceWatcherEventVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceWatcherEvent] {
			fn get_Kind(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceWatcherEventKind) -> ::w::HRESULT,
			fn get_DeviceInformation(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT,
			fn get_DeviceInformationUpdate(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformationUpdate) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceWatcherEvent: ::rt::gen::windows::devices::enumeration::IDeviceWatcherEvent);
		DEFINE_IID!(IID_IDeviceWatcherTriggerDetails, 947945753, 19639, 20055, 165, 109, 119, 109, 7, 203, 254, 249);
		RT_INTERFACE!{interface IDeviceWatcherTriggerDetails(IDeviceWatcherTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceWatcherTriggerDetails] {
			fn get_DeviceWatcherEvents(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::enumeration::DeviceWatcherEvent>) -> ::w::HRESULT
		}}
		RT_CLASS!(DeviceWatcherTriggerDetails: ::rt::gen::windows::devices::enumeration::IDeviceWatcherTriggerDetails);
pub mod pnp { // Windows.Devices.Enumeration.Pnp
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum PnpObjectType: i32 {
			Unknown (PnpObjectType_Unknown) = 0, DeviceInterface (PnpObjectType_DeviceInterface) = 1, DeviceContainer (PnpObjectType_DeviceContainer) = 2, Device (PnpObjectType_Device) = 3, DeviceInterfaceClass (PnpObjectType_DeviceInterfaceClass) = 4, AssociationEndpoint (PnpObjectType_AssociationEndpoint) = 5, AssociationEndpointContainer (PnpObjectType_AssociationEndpointContainer) = 6, AssociationEndpointService (PnpObjectType_AssociationEndpointService) = 7,
		}}
		DEFINE_IID!(IID_IPnpObjectUpdate, 1868163090, 30, 18500, 188, 198, 67, 40, 134, 133, 106, 23);
		RT_INTERFACE!{interface IPnpObjectUpdate(IPnpObjectUpdateVtbl): IInspectable(IInspectableVtbl) [IID_IPnpObjectUpdate] {
			fn get_Type(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::pnp::PnpObjectType) -> ::w::HRESULT,
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT
		}}
		RT_CLASS!(PnpObjectUpdate: ::rt::gen::windows::devices::enumeration::pnp::IPnpObjectUpdate);
		RT_CLASS!(PnpObjectCollection: ::rt::gen::windows::foundation::collections::IVectorView<&'static ::rt::gen::windows::devices::enumeration::pnp::PnpObject>);
		DEFINE_IID!(IID_IPnpObjectWatcher, 2211011752, 18290, 19066, 172, 168, 228, 140, 66, 168, 156, 68);
		RT_INTERFACE!{interface IPnpObjectWatcher(IPnpObjectWatcherVtbl): IInspectable(IInspectableVtbl) [IID_IPnpObjectWatcher] {
			fn add_Added(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher, &::rt::gen::windows::devices::enumeration::pnp::PnpObject>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Added(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Updated(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher, &::rt::gen::windows::devices::enumeration::pnp::PnpObjectUpdate>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Updated(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Removed(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher, &::rt::gen::windows::devices::enumeration::pnp::PnpObjectUpdate>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Removed(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_EnumerationCompleted(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_EnumerationCompleted(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Stopped(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Stopped(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceWatcherStatus) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(PnpObjectWatcher: ::rt::gen::windows::devices::enumeration::pnp::IPnpObjectWatcher);
		RT_CLASS!(PnpObject: ::rt::gen::windows::devices::enumeration::pnp::IPnpObject);
		DEFINE_IID!(IID_IPnpObjectStatics, 3015911997, 53608, 18016, 187, 243, 167, 51, 177, 75, 110, 1);
		RT_INTERFACE!{interface IPnpObjectStatics(IPnpObjectStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPnpObjectStatics] {
			fn CreateFromIdAsync(&mut self, type_: ::rt::gen::windows::devices::enumeration::pnp::PnpObjectType, id: ::w::HSTRING, requestedProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::pnp::PnpObject>) -> ::w::HRESULT,
			fn FindAllAsync(&mut self, type_: ::rt::gen::windows::devices::enumeration::pnp::PnpObjectType, requestedProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::pnp::PnpObjectCollection>) -> ::w::HRESULT,
			fn FindAllAsyncAqsFilter(&mut self, type_: ::rt::gen::windows::devices::enumeration::pnp::PnpObjectType, requestedProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, aqsFilter: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::enumeration::pnp::PnpObjectCollection>) -> ::w::HRESULT,
			fn CreateWatcher(&mut self, type_: ::rt::gen::windows::devices::enumeration::pnp::PnpObjectType, requestedProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher) -> ::w::HRESULT,
			fn CreateWatcherAqsFilter(&mut self, type_: ::rt::gen::windows::devices::enumeration::pnp::PnpObjectType, requestedProperties: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, aqsFilter: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPnpObject, 2512806488, 29499, 19087, 147, 163, 219, 7, 138, 200, 112, 193);
		RT_INTERFACE!{interface IPnpObject(IPnpObjectVtbl): IInspectable(IInspectableVtbl) [IID_IPnpObject] {
			fn get_Type(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::pnp::PnpObjectType) -> ::w::HRESULT,
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT,
			fn Update(&mut self, updateInfo: *mut ::rt::gen::windows::devices::enumeration::pnp::PnpObjectUpdate) -> ::w::HRESULT
		}}
} // Windows.Devices.Enumeration.Pnp
} // Windows.Devices.Enumeration
pub mod geolocation { // Windows.Devices.Geolocation
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum PositionAccuracy: i32 {
			Default (PositionAccuracy_Default) = 0, High (PositionAccuracy_High) = 1,
		}}
		RT_ENUM! { enum PositionStatus: i32 {
			Ready (PositionStatus_Ready) = 0, Initializing (PositionStatus_Initializing) = 1, NoData (PositionStatus_NoData) = 2, Disabled (PositionStatus_Disabled) = 3, NotInitialized (PositionStatus_NotInitialized) = 4, NotAvailable (PositionStatus_NotAvailable) = 5,
		}}
		RT_ENUM! { enum PositionSource: i32 {
			Cellular (PositionSource_Cellular) = 0, Satellite (PositionSource_Satellite) = 1, WiFi (PositionSource_WiFi) = 2, IPAddress (PositionSource_IPAddress) = 3, Unknown (PositionSource_Unknown) = 4,
		}}
		RT_ENUM! { enum GeoshapeType: i32 {
			Geopoint (GeoshapeType_Geopoint) = 0, Geocircle (GeoshapeType_Geocircle) = 1, Geopath (GeoshapeType_Geopath) = 2, GeoboundingBox (GeoshapeType_GeoboundingBox) = 3,
		}}
		RT_ENUM! { enum AltitudeReferenceSystem: i32 {
			Unspecified (AltitudeReferenceSystem_Unspecified) = 0, Terrain (AltitudeReferenceSystem_Terrain) = 1, Ellipsoid (AltitudeReferenceSystem_Ellipsoid) = 2, Geoid (AltitudeReferenceSystem_Geoid) = 3, Surface (AltitudeReferenceSystem_Surface) = 4,
		}}
		RT_STRUCT! { struct BasicGeoposition {
			Latitude: f64, Longitude: f64, Altitude: f64,
		}}
		RT_ENUM! { enum GeolocationAccessStatus: i32 {
			Unspecified (GeolocationAccessStatus_Unspecified) = 0, Allowed (GeolocationAccessStatus_Allowed) = 1, Denied (GeolocationAccessStatus_Denied) = 2,
		}}
		DEFINE_IID!(IID_IGeoshape, 3382485679, 50985, 17345, 143, 171, 214, 222, 201, 20, 223, 126);
		RT_INTERFACE!{interface IGeoshape(IGeoshapeVtbl): IInspectable(IInspectableVtbl) [IID_IGeoshape] {
			fn get_GeoshapeType(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::GeoshapeType) -> ::w::HRESULT,
			fn get_SpatialReferenceId(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_AltitudeReferenceSystem(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeopoint, 1811546347, 58734, 18875, 156, 175, 203, 170, 120, 168, 188, 239);
		RT_INTERFACE!{interface IGeopoint(IGeopointVtbl): IInspectable(IInspectableVtbl) [IID_IGeopoint] {
			fn get_Position(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::BasicGeoposition) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeopointFactory, 3681258803, 30397, 20016, 138, 247, 168, 68, 220, 55, 183, 160);
		RT_INTERFACE!{interface IGeopointFactory(IGeopointFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IGeopointFactory] {
			fn Create(&mut self, position: ::rt::gen::windows::devices::geolocation::BasicGeoposition, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geopoint) -> ::w::HRESULT,
			fn CreateWithAltitudeReferenceSystem(&mut self, position: ::rt::gen::windows::devices::geolocation::BasicGeoposition, altitudeReferenceSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geopoint) -> ::w::HRESULT,
			fn CreateWithAltitudeReferenceSystemAndSpatialReferenceId(&mut self, position: ::rt::gen::windows::devices::geolocation::BasicGeoposition, altitudeReferenceSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, spatialReferenceId: u32, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geopoint) -> ::w::HRESULT
		}}
		RT_CLASS!(Geopoint: ::rt::gen::windows::devices::geolocation::IGeopoint);
		DEFINE_IID!(IID_IGeopath, 3846166457, 11684, 18196, 166, 82, 222, 133, 147, 40, 152, 152);
		RT_INTERFACE!{interface IGeopath(IGeopathVtbl): IInspectable(IInspectableVtbl) [IID_IGeopath] {
			fn get_Positions(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<::rt::gen::windows::devices::geolocation::BasicGeoposition>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeopathFactory, 666806728, 51175, 17241, 155, 155, 252, 163, 224, 94, 245, 147);
		RT_INTERFACE!{interface IGeopathFactory(IGeopathFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IGeopathFactory] {
			fn Create(&mut self, positions: *mut ::rt::gen::windows::foundation::collections::IIterable<::rt::gen::windows::devices::geolocation::BasicGeoposition>, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geopath) -> ::w::HRESULT,
			fn CreateWithAltitudeReference(&mut self, positions: *mut ::rt::gen::windows::foundation::collections::IIterable<::rt::gen::windows::devices::geolocation::BasicGeoposition>, altitudeReferenceSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geopath) -> ::w::HRESULT,
			fn CreateWithAltitudeReferenceAndSpatialReference(&mut self, positions: *mut ::rt::gen::windows::foundation::collections::IIterable<::rt::gen::windows::devices::geolocation::BasicGeoposition>, altitudeReferenceSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, spatialReferenceId: u32, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geopath) -> ::w::HRESULT
		}}
		RT_CLASS!(Geopath: ::rt::gen::windows::devices::geolocation::IGeopath);
		DEFINE_IID!(IID_IGeoboundingBox, 144099339, 10063, 17370, 154, 6, 203, 252, 218, 235, 78, 194);
		RT_INTERFACE!{interface IGeoboundingBox(IGeoboundingBoxVtbl): IInspectable(IInspectableVtbl) [IID_IGeoboundingBox] {
			fn get_NorthwestCorner(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::BasicGeoposition) -> ::w::HRESULT,
			fn get_SoutheastCorner(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::BasicGeoposition) -> ::w::HRESULT,
			fn get_Center(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::BasicGeoposition) -> ::w::HRESULT,
			fn get_MinAltitude(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_MaxAltitude(&mut self, out: *mut f64) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeoboundingBoxFactory, 1308337545, 1041, 19132, 179, 181, 91, 188, 203, 87, 217, 140);
		RT_INTERFACE!{interface IGeoboundingBoxFactory(IGeoboundingBoxFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IGeoboundingBoxFactory] {
			fn Create(&mut self, northwestCorner: ::rt::gen::windows::devices::geolocation::BasicGeoposition, southeastCorner: ::rt::gen::windows::devices::geolocation::BasicGeoposition, out: *mut *mut ::rt::gen::windows::devices::geolocation::GeoboundingBox) -> ::w::HRESULT,
			fn CreateWithAltitudeReference(&mut self, northwestCorner: ::rt::gen::windows::devices::geolocation::BasicGeoposition, southeastCorner: ::rt::gen::windows::devices::geolocation::BasicGeoposition, altitudeReferenceSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, out: *mut *mut ::rt::gen::windows::devices::geolocation::GeoboundingBox) -> ::w::HRESULT,
			fn CreateWithAltitudeReferenceAndSpatialReference(&mut self, northwestCorner: ::rt::gen::windows::devices::geolocation::BasicGeoposition, southeastCorner: ::rt::gen::windows::devices::geolocation::BasicGeoposition, altitudeReferenceSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, spatialReferenceId: u32, out: *mut *mut ::rt::gen::windows::devices::geolocation::GeoboundingBox) -> ::w::HRESULT
		}}
		RT_CLASS!(GeoboundingBox: ::rt::gen::windows::devices::geolocation::IGeoboundingBox);
		DEFINE_IID!(IID_IGeoboundingBoxStatics, 1740113672, 58906, 19664, 132, 27, 147, 35, 55, 146, 181, 202);
		RT_INTERFACE!{interface IGeoboundingBoxStatics(IGeoboundingBoxStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGeoboundingBoxStatics] {
			fn TryCompute(&mut self, positions: *mut ::rt::gen::windows::foundation::collections::IIterable<::rt::gen::windows::devices::geolocation::BasicGeoposition>, out: *mut *mut ::rt::gen::windows::devices::geolocation::GeoboundingBox) -> ::w::HRESULT,
			fn TryComputeWithAltitudeReference(&mut self, positions: *mut ::rt::gen::windows::foundation::collections::IIterable<::rt::gen::windows::devices::geolocation::BasicGeoposition>, altitudeRefSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, out: *mut *mut ::rt::gen::windows::devices::geolocation::GeoboundingBox) -> ::w::HRESULT,
			fn TryComputeWithAltitudeReferenceAndSpatialReference(&mut self, positions: *mut ::rt::gen::windows::foundation::collections::IIterable<::rt::gen::windows::devices::geolocation::BasicGeoposition>, altitudeRefSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, spatialReferenceId: u32, out: *mut *mut ::rt::gen::windows::devices::geolocation::GeoboundingBox) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeocoordinateSatelliteData, 3274339545, 9736, 18252, 145, 44, 6, 221, 73, 15, 74, 247);
		RT_INTERFACE!{interface IGeocoordinateSatelliteData(IGeocoordinateSatelliteDataVtbl): IInspectable(IInspectableVtbl) [IID_IGeocoordinateSatelliteData] {
			fn get_PositionDilutionOfPrecision(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<f64>) -> ::w::HRESULT,
			fn get_HorizontalDilutionOfPrecision(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<f64>) -> ::w::HRESULT,
			fn get_VerticalDilutionOfPrecision(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<f64>) -> ::w::HRESULT
		}}
		RT_CLASS!(GeocoordinateSatelliteData: ::rt::gen::windows::devices::geolocation::IGeocoordinateSatelliteData);
		DEFINE_IID!(IID_IVenueData, 1727238535, 24803, 19247, 181, 39, 79, 83, 241, 195, 198, 119);
		RT_INTERFACE!{interface IVenueData(IVenueDataVtbl): IInspectable(IInspectableVtbl) [IID_IVenueData] {
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Level(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(VenueData: ::rt::gen::windows::devices::geolocation::IVenueData);
		DEFINE_IID!(IID_IGeocoordinate, 3995181994, 38762, 19568, 128, 61, 8, 62, 165, 91, 203, 196);
		RT_INTERFACE!{interface IGeocoordinate(IGeocoordinateVtbl): IInspectable(IInspectableVtbl) [IID_IGeocoordinate] {
			fn get_Latitude(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_Longitude(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_Altitude(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<f64>) -> ::w::HRESULT,
			fn get_Accuracy(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_AltitudeAccuracy(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<f64>) -> ::w::HRESULT,
			fn get_Heading(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<f64>) -> ::w::HRESULT,
			fn get_Speed(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<f64>) -> ::w::HRESULT,
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeocoordinateWithPositionData, 2514891966, 56278, 16556, 184, 242, 166, 92, 3, 64, 217, 166);
		RT_INTERFACE!{interface IGeocoordinateWithPositionData(IGeocoordinateWithPositionDataVtbl): IInspectable(IInspectableVtbl) [IID_IGeocoordinateWithPositionData] {
			fn get_PositionSource(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::PositionSource) -> ::w::HRESULT,
			fn get_SatelliteData(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::GeocoordinateSatelliteData) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeocoordinateWithPoint, 4276749605, 53804, 19782, 181, 39, 11, 150, 6, 111, 199, 219);
		RT_INTERFACE!{interface IGeocoordinateWithPoint(IGeocoordinateWithPointVtbl): IInspectable(IInspectableVtbl) [IID_IGeocoordinateWithPoint] {
			fn get_Point(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geopoint) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeocoordinateWithPositionSourceTimestamp, 2235825154, 51697, 17936, 175, 224, 139, 195, 166, 168, 112, 54);
		RT_INTERFACE!{interface IGeocoordinateWithPositionSourceTimestamp(IGeocoordinateWithPositionSourceTimestampVtbl): IInspectable(IInspectableVtbl) [IID_IGeocoordinateWithPositionSourceTimestamp] {
			fn get_PositionSourceTimestamp(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::DateTime>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeoposition, 3247244372, 32065, 20471, 169, 87, 157, 255, 180, 239, 127, 91);
		RT_INTERFACE!{interface IGeoposition(IGeopositionVtbl): IInspectable(IInspectableVtbl) [IID_IGeoposition] {
			fn get_Coordinate(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geocoordinate) -> ::w::HRESULT,
			fn get_CivicAddress(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::CivicAddress) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeoposition2, 2137192087, 34417, 19213, 134, 248, 71, 74, 132, 150, 24, 124);
		RT_INTERFACE!{interface IGeoposition2(IGeoposition2Vtbl): IInspectable(IInspectableVtbl) [IID_IGeoposition2] {
			fn get_VenueData(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::VenueData) -> ::w::HRESULT
		}}
		RT_CLASS!(Geocoordinate: ::rt::gen::windows::devices::geolocation::IGeocoordinate);
		RT_CLASS!(CivicAddress: ::rt::gen::windows::devices::geolocation::ICivicAddress);
		DEFINE_IID!(IID_ICivicAddress, 2824239642, 25844, 19784, 188, 234, 246, 176, 8, 236, 163, 76);
		RT_INTERFACE!{interface ICivicAddress(ICivicAddressVtbl): IInspectable(IInspectableVtbl) [IID_ICivicAddress] {
			fn get_Country(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_State(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_City(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_PostalCode(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT
		}}
		RT_CLASS!(Geoposition: ::rt::gen::windows::devices::geolocation::IGeoposition);
		DEFINE_IID!(IID_IPositionChangedEventArgs, 931503333, 40222, 18117, 191, 59, 106, 216, 202, 193, 160, 147);
		RT_INTERFACE!{interface IPositionChangedEventArgs(IPositionChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPositionChangedEventArgs] {
			fn get_Position(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geoposition) -> ::w::HRESULT
		}}
		RT_CLASS!(PositionChangedEventArgs: ::rt::gen::windows::devices::geolocation::IPositionChangedEventArgs);
		DEFINE_IID!(IID_IStatusChangedEventArgs, 877908698, 35987, 16657, 162, 5, 154, 236, 252, 155, 229, 192);
		RT_INTERFACE!{interface IStatusChangedEventArgs(IStatusChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IStatusChangedEventArgs] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::PositionStatus) -> ::w::HRESULT
		}}
		RT_CLASS!(StatusChangedEventArgs: ::rt::gen::windows::devices::geolocation::IStatusChangedEventArgs);
		DEFINE_IID!(IID_IGeolocator, 2848178018, 17700, 18825, 138, 169, 222, 1, 157, 46, 85, 31);
		RT_INTERFACE!{interface IGeolocator(IGeolocatorVtbl): IInspectable(IInspectableVtbl) [IID_IGeolocator] {
			fn get_DesiredAccuracy(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::PositionAccuracy) -> ::w::HRESULT,
			fn put_DesiredAccuracy(&mut self, value: ::rt::gen::windows::devices::geolocation::PositionAccuracy) -> ::w::HRESULT,
			fn get_MovementThreshold(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn put_MovementThreshold(&mut self, value: f64) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_LocationStatus(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::PositionStatus) -> ::w::HRESULT,
			fn GetGeopositionAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::geolocation::Geoposition>) -> ::w::HRESULT,
			fn GetGeopositionAsyncWithAgeAndTimeout(&mut self, maximumAge: ::rt::gen::windows::foundation::TimeSpan, timeout: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::geolocation::Geoposition>) -> ::w::HRESULT,
			fn add_PositionChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::geolocation::Geolocator, &::rt::gen::windows::devices::geolocation::PositionChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_PositionChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_StatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::geolocation::Geolocator, &::rt::gen::windows::devices::geolocation::StatusChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(Geolocator: ::rt::gen::windows::devices::geolocation::IGeolocator);
		DEFINE_IID!(IID_IGeolocatorWithScalarAccuracy, 2532692929, 47119, 17930, 153, 77, 169, 108, 71, 165, 26, 164);
		RT_INTERFACE!{interface IGeolocatorWithScalarAccuracy(IGeolocatorWithScalarAccuracyVtbl): IInspectable(IInspectableVtbl) [IID_IGeolocatorWithScalarAccuracy] {
			fn get_DesiredAccuracyInMeters(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u32>) -> ::w::HRESULT,
			fn put_DesiredAccuracyInMeters(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<u32>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeolocatorStatics, 2593027441, 11765, 17809, 159, 135, 235, 95, 216, 148, 233, 183);
		RT_INTERFACE!{interface IGeolocatorStatics(IGeolocatorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGeolocatorStatics] {
			fn RequestAccessAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::geolocation::GeolocationAccessStatus>) -> ::w::HRESULT,
			fn GetGeopositionHistoryAsync(&mut self, startTime: ::rt::gen::windows::foundation::DateTime, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::geolocation::Geoposition>>) -> ::w::HRESULT,
			fn GetGeopositionHistoryWithDurationAsync(&mut self, startTime: ::rt::gen::windows::foundation::DateTime, duration: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::geolocation::Geoposition>>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeocircle, 971266115, 43001, 20067, 146, 167, 186, 12, 40, 209, 36, 177);
		RT_INTERFACE!{interface IGeocircle(IGeocircleVtbl): IInspectable(IInspectableVtbl) [IID_IGeocircle] {
			fn get_Center(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::BasicGeoposition) -> ::w::HRESULT,
			fn get_Radius(&mut self, out: *mut f64) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeocircleFactory, 2950058783, 29361, 20349, 135, 204, 78, 212, 201, 132, 156, 5);
		RT_INTERFACE!{interface IGeocircleFactory(IGeocircleFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IGeocircleFactory] {
			fn Create(&mut self, position: ::rt::gen::windows::devices::geolocation::BasicGeoposition, radius: f64, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geocircle) -> ::w::HRESULT,
			fn CreateWithAltitudeReferenceSystem(&mut self, position: ::rt::gen::windows::devices::geolocation::BasicGeoposition, radius: f64, altitudeReferenceSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geocircle) -> ::w::HRESULT,
			fn CreateWithAltitudeReferenceSystemAndSpatialReferenceId(&mut self, position: ::rt::gen::windows::devices::geolocation::BasicGeoposition, radius: f64, altitudeReferenceSystem: ::rt::gen::windows::devices::geolocation::AltitudeReferenceSystem, spatialReferenceId: u32, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geocircle) -> ::w::HRESULT
		}}
		RT_CLASS!(Geocircle: ::rt::gen::windows::devices::geolocation::IGeocircle);
pub mod geofencing { // Windows.Devices.Geolocation.Geofencing
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum MonitoredGeofenceStates: u32 {
			None (MonitoredGeofenceStates_None) = 0, Entered (MonitoredGeofenceStates_Entered) = 1, Exited (MonitoredGeofenceStates_Exited) = 2, Removed (MonitoredGeofenceStates_Removed) = 4,
		}}
		RT_ENUM! { enum GeofenceState: u32 {
			None (GeofenceState_None) = 0, Entered (GeofenceState_Entered) = 1, Exited (GeofenceState_Exited) = 2, Removed (GeofenceState_Removed) = 4,
		}}
		RT_ENUM! { enum GeofenceMonitorStatus: i32 {
			Ready (GeofenceMonitorStatus_Ready) = 0, Initializing (GeofenceMonitorStatus_Initializing) = 1, NoData (GeofenceMonitorStatus_NoData) = 2, Disabled (GeofenceMonitorStatus_Disabled) = 3, NotInitialized (GeofenceMonitorStatus_NotInitialized) = 4, NotAvailable (GeofenceMonitorStatus_NotAvailable) = 5,
		}}
		RT_ENUM! { enum GeofenceRemovalReason: i32 {
			Used (GeofenceRemovalReason_Used) = 0, Expired (GeofenceRemovalReason_Expired) = 1,
		}}
		DEFINE_IID!(IID_IGeofenceFactory, 2216649291, 12895, 19344, 188, 167, 43, 128, 34, 169, 55, 150);
		RT_INTERFACE!{interface IGeofenceFactory(IGeofenceFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IGeofenceFactory] {
			fn Create(&mut self, id: ::w::HSTRING, geoshape: *mut ::rt::gen::windows::devices::geolocation::IGeoshape, out: *mut *mut ::rt::gen::windows::devices::geolocation::geofencing::Geofence) -> ::w::HRESULT,
			fn CreateWithMonitorStates(&mut self, id: ::w::HSTRING, geoshape: *mut ::rt::gen::windows::devices::geolocation::IGeoshape, monitoredStates: ::rt::gen::windows::devices::geolocation::geofencing::MonitoredGeofenceStates, singleUse: ::w::BOOL, out: *mut *mut ::rt::gen::windows::devices::geolocation::geofencing::Geofence) -> ::w::HRESULT,
			fn CreateWithMonitorStatesAndDwellTime(&mut self, id: ::w::HSTRING, geoshape: *mut ::rt::gen::windows::devices::geolocation::IGeoshape, monitoredStates: ::rt::gen::windows::devices::geolocation::geofencing::MonitoredGeofenceStates, singleUse: ::w::BOOL, dwellTime: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut ::rt::gen::windows::devices::geolocation::geofencing::Geofence) -> ::w::HRESULT,
			fn CreateWithMonitorStatesDwellTimeStartTimeAndDuration(&mut self, id: ::w::HSTRING, geoshape: *mut ::rt::gen::windows::devices::geolocation::IGeoshape, monitoredStates: ::rt::gen::windows::devices::geolocation::geofencing::MonitoredGeofenceStates, singleUse: ::w::BOOL, dwellTime: ::rt::gen::windows::foundation::TimeSpan, startTime: ::rt::gen::windows::foundation::DateTime, duration: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut ::rt::gen::windows::devices::geolocation::geofencing::Geofence) -> ::w::HRESULT
		}}
		RT_CLASS!(Geofence: ::rt::gen::windows::devices::geolocation::geofencing::IGeofence);
		DEFINE_IID!(IID_IGeofence, 2617837603, 60856, 18400, 130, 69, 91, 246, 29, 50, 31, 45);
		RT_INTERFACE!{interface IGeofence(IGeofenceVtbl): IInspectable(IInspectableVtbl) [IID_IGeofence] {
			fn get_StartTime(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_Duration(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_DwellTime(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_MonitoredStates(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::geofencing::MonitoredGeofenceStates) -> ::w::HRESULT,
			fn get_Geoshape(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::IGeoshape) -> ::w::HRESULT,
			fn get_SingleUse(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeofenceStateChangeReport, 2586065944, 9316, 19593, 190, 5, 179, 255, 255, 91, 171, 197);
		RT_INTERFACE!{interface IGeofenceStateChangeReport(IGeofenceStateChangeReportVtbl): IInspectable(IInspectableVtbl) [IID_IGeofenceStateChangeReport] {
			fn get_NewState(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::geofencing::GeofenceState) -> ::w::HRESULT,
			fn get_Geofence(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::geofencing::Geofence) -> ::w::HRESULT,
			fn get_Geoposition(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geoposition) -> ::w::HRESULT,
			fn get_RemovalReason(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::geofencing::GeofenceRemovalReason) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGeofenceMonitorStatics, 768815055, 32373, 18585, 172, 227, 43, 208, 166, 92, 206, 6);
		RT_INTERFACE!{interface IGeofenceMonitorStatics(IGeofenceMonitorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGeofenceMonitorStatics] {
			fn get_Current(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::geofencing::GeofenceMonitor) -> ::w::HRESULT
		}}
		RT_CLASS!(GeofenceMonitor: ::rt::gen::windows::devices::geolocation::geofencing::IGeofenceMonitor);
		DEFINE_IID!(IID_IGeofenceMonitor, 1276075896, 7199, 17953, 187, 189, 131, 59, 146, 36, 114, 38);
		RT_INTERFACE!{interface IGeofenceMonitor(IGeofenceMonitorVtbl): IInspectable(IInspectableVtbl) [IID_IGeofenceMonitor] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::geolocation::geofencing::GeofenceMonitorStatus) -> ::w::HRESULT,
			fn get_Geofences(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::geolocation::geofencing::Geofence>) -> ::w::HRESULT,
			fn get_LastKnownGeoposition(&mut self, out: *mut *mut ::rt::gen::windows::devices::geolocation::Geoposition) -> ::w::HRESULT,
			fn add_GeofenceStateChanged(&mut self, eventHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::geolocation::geofencing::GeofenceMonitor, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_GeofenceStateChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn ReadReports(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::geolocation::geofencing::GeofenceStateChangeReport>) -> ::w::HRESULT,
			fn add_StatusChanged(&mut self, eventHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::geolocation::geofencing::GeofenceMonitor, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(GeofenceStateChangeReport: ::rt::gen::windows::devices::geolocation::geofencing::IGeofenceStateChangeReport);
} // Windows.Devices.Geolocation.Geofencing
} // Windows.Devices.Geolocation
pub mod humaninterfacedevice { // Windows.Devices.HumanInterfaceDevice
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum HidReportType: i32 {
			Input (HidReportType_Input) = 0, Output (HidReportType_Output) = 1, Feature (HidReportType_Feature) = 2,
		}}
		RT_ENUM! { enum HidCollectionType: i32 {
			Physical (HidCollectionType_Physical) = 0, Application (HidCollectionType_Application) = 1, Logical (HidCollectionType_Logical) = 2, Report (HidCollectionType_Report) = 3, NamedArray (HidCollectionType_NamedArray) = 4, UsageSwitch (HidCollectionType_UsageSwitch) = 5, UsageModifier (HidCollectionType_UsageModifier) = 6, Other (HidCollectionType_Other) = 7,
		}}
		DEFINE_IID!(IID_IHidDeviceStatics, 2656666084, 38998, 16780, 159, 115, 119, 222, 12, 216, 87, 84);
		RT_INTERFACE!{interface IHidDeviceStatics(IHidDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IHidDeviceStatics] {
			fn GetDeviceSelector(&mut self, usagePage: u16, usageId: u16, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorVidPid(&mut self, usagePage: u16, usageId: u16, vendorId: u16, productId: u16, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, accessMode: ::rt::gen::windows::storage::FileAccessMode, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::humaninterfacedevice::HidDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(HidDevice: ::rt::gen::windows::devices::humaninterfacedevice::IHidDevice);
		RT_CLASS!(HidInputReport: ::rt::gen::windows::devices::humaninterfacedevice::IHidInputReport);
		RT_CLASS!(HidFeatureReport: ::rt::gen::windows::devices::humaninterfacedevice::IHidFeatureReport);
		RT_CLASS!(HidOutputReport: ::rt::gen::windows::devices::humaninterfacedevice::IHidOutputReport);
		RT_CLASS!(HidBooleanControlDescription: ::rt::gen::windows::devices::humaninterfacedevice::IHidBooleanControlDescription);
		RT_CLASS!(HidNumericControlDescription: ::rt::gen::windows::devices::humaninterfacedevice::IHidNumericControlDescription);
		RT_CLASS!(HidInputReportReceivedEventArgs: ::rt::gen::windows::devices::humaninterfacedevice::IHidInputReportReceivedEventArgs);
		DEFINE_IID!(IID_IHidBooleanControlDescription, 1637279043, 10712, 18986, 134, 131, 132, 158, 32, 123, 190, 49);
		RT_INTERFACE!{interface IHidBooleanControlDescription(IHidBooleanControlDescriptionVtbl): IInspectable(IInspectableVtbl) [IID_IHidBooleanControlDescription] {
			fn get_Id(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ReportId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_ReportType(&mut self, out: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidReportType) -> ::w::HRESULT,
			fn get_UsagePage(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_UsageId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_ParentCollections(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::humaninterfacedevice::HidCollection>) -> ::w::HRESULT
		}}
		RT_CLASS!(HidCollection: ::rt::gen::windows::devices::humaninterfacedevice::IHidCollection);
		DEFINE_IID!(IID_IHidNumericControlDescription, 1670209158, 7575, 19573, 146, 127, 95, 245, 139, 160, 94, 50);
		RT_INTERFACE!{interface IHidNumericControlDescription(IHidNumericControlDescriptionVtbl): IInspectable(IInspectableVtbl) [IID_IHidNumericControlDescription] {
			fn get_Id(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ReportId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_ReportType(&mut self, out: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidReportType) -> ::w::HRESULT,
			fn get_ReportSize(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ReportCount(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UsagePage(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_UsageId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_LogicalMinimum(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_LogicalMaximum(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_PhysicalMinimum(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_PhysicalMaximum(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_UnitExponent(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Unit(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_IsAbsolute(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_HasNull(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_ParentCollections(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::humaninterfacedevice::HidCollection>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IHidCollection, 1904866723, 13041, 18147, 190, 253, 68, 210, 102, 59, 126, 106);
		RT_INTERFACE!{interface IHidCollection(IHidCollectionVtbl): IInspectable(IInspectableVtbl) [IID_IHidCollection] {
			fn get_Id(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Type(&mut self, out: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidCollectionType) -> ::w::HRESULT,
			fn get_UsagePage(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UsageId(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IHidInputReport, 3277655632, 63463, 20109, 178, 62, 202, 187, 229, 107, 144, 233);
		RT_INTERFACE!{interface IHidInputReport(IHidInputReportVtbl): IInspectable(IInspectableVtbl) [IID_IHidInputReport] {
			fn get_Id(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Data(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_ActivatedBooleanControls(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl>) -> ::w::HRESULT,
			fn get_TransitionedBooleanControls(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl>) -> ::w::HRESULT,
			fn GetBooleanControl(&mut self, usagePage: u16, usageId: u16, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl) -> ::w::HRESULT,
			fn GetBooleanControlByDescription(&mut self, controlDescription: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControlDescription, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl) -> ::w::HRESULT,
			fn GetNumericControl(&mut self, usagePage: u16, usageId: u16, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControl) -> ::w::HRESULT,
			fn GetNumericControlByDescription(&mut self, controlDescription: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControlDescription, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControl) -> ::w::HRESULT
		}}
		RT_CLASS!(HidBooleanControl: ::rt::gen::windows::devices::humaninterfacedevice::IHidBooleanControl);
		RT_CLASS!(HidNumericControl: ::rt::gen::windows::devices::humaninterfacedevice::IHidNumericControl);
		DEFINE_IID!(IID_IHidOutputReport, 1657480516, 51350, 17507, 147, 193, 223, 157, 176, 83, 196, 80);
		RT_INTERFACE!{interface IHidOutputReport(IHidOutputReportVtbl): IInspectable(IInspectableVtbl) [IID_IHidOutputReport] {
			fn get_Id(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Data(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_Data(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn GetBooleanControl(&mut self, usagePage: u16, usageId: u16, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl) -> ::w::HRESULT,
			fn GetBooleanControlByDescription(&mut self, controlDescription: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControlDescription, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl) -> ::w::HRESULT,
			fn GetNumericControl(&mut self, usagePage: u16, usageId: u16, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControl) -> ::w::HRESULT,
			fn GetNumericControlByDescription(&mut self, controlDescription: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControlDescription, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControl) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IHidFeatureReport, 2216532857, 23269, 18147, 130, 239, 31, 236, 92, 137, 66, 244);
		RT_INTERFACE!{interface IHidFeatureReport(IHidFeatureReportVtbl): IInspectable(IInspectableVtbl) [IID_IHidFeatureReport] {
			fn get_Id(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Data(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_Data(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn GetBooleanControl(&mut self, usagePage: u16, usageId: u16, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl) -> ::w::HRESULT,
			fn GetBooleanControlByDescription(&mut self, controlDescription: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControlDescription, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl) -> ::w::HRESULT,
			fn GetNumericControl(&mut self, usagePage: u16, usageId: u16, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControl) -> ::w::HRESULT,
			fn GetNumericControlByDescription(&mut self, controlDescription: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControlDescription, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControl) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IHidInputReportReceivedEventArgs, 1884931531, 22962, 19906, 152, 92, 10, 220, 97, 54, 250, 45);
		RT_INTERFACE!{interface IHidInputReportReceivedEventArgs(IHidInputReportReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IHidInputReportReceivedEventArgs] {
			fn get_Report(&mut self, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidInputReport) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IHidBooleanControl, 1380840586, 13973, 16524, 187, 162, 226, 235, 90, 191, 188, 32);
		RT_INTERFACE!{interface IHidBooleanControl(IHidBooleanControlVtbl): IInspectable(IInspectableVtbl) [IID_IHidBooleanControl] {
			fn get_Id(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UsagePage(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_UsageId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_IsActive(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsActive(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_ControlDescription(&mut self, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControlDescription) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IHidNumericControl, 3817476773, 13735, 19317, 137, 200, 251, 31, 40, 177, 8, 35);
		RT_INTERFACE!{interface IHidNumericControl(IHidNumericControlVtbl): IInspectable(IInspectableVtbl) [IID_IHidNumericControl] {
			fn get_Id(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_IsGrouped(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_UsagePage(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_UsageId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Value(&mut self, out: *mut i64) -> ::w::HRESULT,
			fn put_Value(&mut self, value: i64) -> ::w::HRESULT,
			fn get_ScaledValue(&mut self, out: *mut i64) -> ::w::HRESULT,
			fn put_ScaledValue(&mut self, value: i64) -> ::w::HRESULT,
			fn get_ControlDescription(&mut self, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControlDescription) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IHidDevice, 1602884839, 8704, 17198, 149, 218, 208, 155, 135, 213, 116, 168);
		RT_INTERFACE!{interface IHidDevice(IHidDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IHidDevice] {
			fn get_VendorId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_ProductId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_Version(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_UsagePage(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_UsageId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn GetInputReportAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::humaninterfacedevice::HidInputReport>) -> ::w::HRESULT,
			fn GetInputReportByIdAsync(&mut self, reportId: u16, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::humaninterfacedevice::HidInputReport>) -> ::w::HRESULT,
			fn GetFeatureReportAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::humaninterfacedevice::HidFeatureReport>) -> ::w::HRESULT,
			fn GetFeatureReportByIdAsync(&mut self, reportId: u16, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::humaninterfacedevice::HidFeatureReport>) -> ::w::HRESULT,
			fn CreateOutputReport(&mut self, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidOutputReport) -> ::w::HRESULT,
			fn CreateOutputReportById(&mut self, reportId: u16, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidOutputReport) -> ::w::HRESULT,
			fn CreateFeatureReport(&mut self, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidFeatureReport) -> ::w::HRESULT,
			fn CreateFeatureReportById(&mut self, reportId: u16, out: *mut *mut ::rt::gen::windows::devices::humaninterfacedevice::HidFeatureReport) -> ::w::HRESULT,
			fn SendOutputReportAsync(&mut self, outputReport: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidOutputReport, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<u32>) -> ::w::HRESULT,
			fn SendFeatureReportAsync(&mut self, featureReport: *mut ::rt::gen::windows::devices::humaninterfacedevice::HidFeatureReport, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<u32>) -> ::w::HRESULT,
			fn GetBooleanControlDescriptions(&mut self, reportType: ::rt::gen::windows::devices::humaninterfacedevice::HidReportType, usagePage: u16, usageId: u16, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControlDescription>) -> ::w::HRESULT,
			fn GetNumericControlDescriptions(&mut self, reportType: ::rt::gen::windows::devices::humaninterfacedevice::HidReportType, usagePage: u16, usageId: u16, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::humaninterfacedevice::HidNumericControlDescription>) -> ::w::HRESULT,
			fn add_InputReportReceived(&mut self, reportHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::humaninterfacedevice::HidDevice, &::rt::gen::windows::devices::humaninterfacedevice::HidInputReportReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_InputReportReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
} // Windows.Devices.HumanInterfaceDevice
pub mod input { // Windows.Devices.Input
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum PointerDeviceType: i32 {
			Touch (PointerDeviceType_Touch) = 0, Pen (PointerDeviceType_Pen) = 1, Mouse (PointerDeviceType_Mouse) = 2,
		}}
		RT_STRUCT! { struct PointerDeviceUsage {
			UsagePage: u32, Usage: u32, MinLogical: i32, MaxLogical: i32, MinPhysical: i32, MaxPhysical: i32, Unit: u32, PhysicalMultiplier: f32,
		}}
		RT_STRUCT! { struct MouseDelta {
			X: i32, Y: i32,
		}}
		DEFINE_IID!(IID_IMouseCapabilities, 3164987427, 32217, 19307, 154, 146, 85, 212, 60, 179, 143, 115);
		RT_INTERFACE!{interface IMouseCapabilities(IMouseCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_IMouseCapabilities] {
			fn get_MousePresent(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_VerticalWheelPresent(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_HorizontalWheelPresent(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_SwapButtons(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_NumberOfButtons(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKeyboardCapabilities, 977247062, 26520, 19388, 131, 62, 15, 52, 177, 124, 101, 255);
		RT_INTERFACE!{interface IKeyboardCapabilities(IKeyboardCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_IKeyboardCapabilities] {
			fn get_KeyboardPresent(&mut self, out: *mut i32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ITouchCapabilities, 551376377, 5105, 18120, 146, 133, 44, 5, 250, 62, 218, 111);
		RT_INTERFACE!{interface ITouchCapabilities(ITouchCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_ITouchCapabilities] {
			fn get_TouchPresent(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Contacts(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPointerDeviceStatics, 3635976865, 53702, 16750, 189, 141, 87, 144, 145, 77, 197, 99);
		RT_INTERFACE!{interface IPointerDeviceStatics(IPointerDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPointerDeviceStatics] {
			fn GetPointerDevice(&mut self, pointerId: u32, out: *mut *mut ::rt::gen::windows::devices::input::PointerDevice) -> ::w::HRESULT,
			fn GetPointerDevices(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::input::PointerDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(PointerDevice: ::rt::gen::windows::devices::input::IPointerDevice);
		DEFINE_IID!(IID_IPointerDevice, 2479471356, 60363, 18046, 130, 198, 39, 111, 234, 227, 107, 90);
		RT_INTERFACE!{interface IPointerDevice(IPointerDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IPointerDevice] {
			fn get_PointerDeviceType(&mut self, out: *mut ::rt::gen::windows::devices::input::PointerDeviceType) -> ::w::HRESULT,
			fn get_IsIntegrated(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_MaxContacts(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_PhysicalDeviceRect(&mut self, out: *mut ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn get_ScreenRect(&mut self, out: *mut ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn get_SupportedUsages(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<::rt::gen::windows::devices::input::PointerDeviceUsage>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPointerDevice2, 4171682464, 50308, 18591, 174, 62, 48, 210, 238, 31, 253, 62);
		RT_INTERFACE!{interface IPointerDevice2(IPointerDevice2Vtbl): IInspectable(IInspectableVtbl) [IID_IPointerDevice2] {
			fn get_MaxPointersWithZDistance(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMouseEventArgs, 4129663581, 9044, 19655, 146, 48, 150, 148, 28, 150, 159, 222);
		RT_INTERFACE!{interface IMouseEventArgs(IMouseEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMouseEventArgs] {
			fn get_MouseDelta(&mut self, out: *mut ::rt::gen::windows::devices::input::MouseDelta) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMouseDevice, 2297295960, 62152, 18932, 190, 31, 194, 86, 179, 136, 188, 17);
		RT_INTERFACE!{interface IMouseDevice(IMouseDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IMouseDevice] {
			fn add_MouseMoved(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::input::MouseDevice, &::rt::gen::windows::devices::input::MouseEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_MouseMoved(&mut self, cookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(MouseDevice: ::rt::gen::windows::devices::input::IMouseDevice);
		RT_CLASS!(MouseEventArgs: ::rt::gen::windows::devices::input::IMouseEventArgs);
		DEFINE_IID!(IID_IMouseDeviceStatics, 1212846149, 28016, 18907, 142, 104, 70, 255, 189, 23, 211, 141);
		RT_INTERFACE!{interface IMouseDeviceStatics(IMouseDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IMouseDeviceStatics] {
			fn GetForCurrentView(&mut self, out: *mut *mut ::rt::gen::windows::devices::input::MouseDevice) -> ::w::HRESULT
		}}
		RT_CLASS!(MouseCapabilities: ::rt::gen::windows::devices::input::IMouseCapabilities);
		RT_CLASS!(KeyboardCapabilities: ::rt::gen::windows::devices::input::IKeyboardCapabilities);
		RT_CLASS!(TouchCapabilities: ::rt::gen::windows::devices::input::ITouchCapabilities);
} // Windows.Devices.Input
pub mod lights { // Windows.Devices.Lights
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_ILampStatics, 2820817260, 34949, 16414, 184, 33, 142, 139, 56, 168, 232, 236);
		RT_INTERFACE!{interface ILampStatics(ILampStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ILampStatics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::lights::Lamp>) -> ::w::HRESULT,
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::lights::Lamp>) -> ::w::HRESULT
		}}
		RT_CLASS!(Lamp: ::rt::gen::windows::devices::lights::ILamp);
		RT_CLASS!(LampAvailabilityChangedEventArgs: ::rt::gen::windows::devices::lights::ILampAvailabilityChangedEventArgs);
		DEFINE_IID!(IID_ILampAvailabilityChangedEventArgs, 1332624877, 1954, 18845, 146, 96, 103, 227, 4, 83, 43, 164);
		RT_INTERFACE!{interface ILampAvailabilityChangedEventArgs(ILampAvailabilityChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ILampAvailabilityChangedEventArgs] {
			fn get_IsAvailable(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILamp, 75324314, 59973, 19243, 177, 162, 20, 223, 240, 11, 222, 123);
		RT_INTERFACE!{interface ILamp(ILampVtbl): IInspectable(IInspectableVtbl) [IID_ILamp] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_BrightnessLevel(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn put_BrightnessLevel(&mut self, value: f32) -> ::w::HRESULT,
			fn get_IsColorSettable(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Color(&mut self, out: *mut ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn put_Color(&mut self, value: ::rt::gen::windows::ui::Color) -> ::w::HRESULT,
			fn add_AvailabilityChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::lights::Lamp, &::rt::gen::windows::devices::lights::LampAvailabilityChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AvailabilityChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
} // Windows.Devices.Lights
pub mod midi { // Windows.Devices.Midi
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum MidiMessageType: i32 {
			None (MidiMessageType_None) = 0, NoteOff (MidiMessageType_NoteOff) = 128, NoteOn (MidiMessageType_NoteOn) = 144, PolyphonicKeyPressure (MidiMessageType_PolyphonicKeyPressure) = 160, ControlChange (MidiMessageType_ControlChange) = 176, ProgramChange (MidiMessageType_ProgramChange) = 192, ChannelPressure (MidiMessageType_ChannelPressure) = 208, PitchBendChange (MidiMessageType_PitchBendChange) = 224, SystemExclusive (MidiMessageType_SystemExclusive) = 240, MidiTimeCode (MidiMessageType_MidiTimeCode) = 241, SongPositionPointer (MidiMessageType_SongPositionPointer) = 242, SongSelect (MidiMessageType_SongSelect) = 243, TuneRequest (MidiMessageType_TuneRequest) = 246, TimingClock (MidiMessageType_TimingClock) = 248, Start (MidiMessageType_Start) = 250, Continue (MidiMessageType_Continue) = 251, Stop (MidiMessageType_Stop) = 252, ActiveSensing (MidiMessageType_ActiveSensing) = 254, SystemReset (MidiMessageType_SystemReset) = 255,
		}}
		DEFINE_IID!(IID_IMidiMessage, 2037807429, 4244, 17027, 155, 224, 40, 159, 192, 238, 131, 52);
		RT_INTERFACE!{interface IMidiMessage(IMidiMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiMessage] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_RawData(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_Type(&mut self, out: *mut ::rt::gen::windows::devices::midi::MidiMessageType) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiNoteOffMessage, 385714932, 6542, 19855, 166, 84, 211, 5, 162, 147, 84, 143);
		RT_INTERFACE!{interface IMidiNoteOffMessage(IMidiNoteOffMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiNoteOffMessage] {
			fn get_Channel(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Note(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Velocity(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiNoteOnMessage, 3760343797, 24961, 18141, 175, 162, 65, 0, 4, 192, 87, 170);
		RT_INTERFACE!{interface IMidiNoteOnMessage(IMidiNoteOnMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiNoteOnMessage] {
			fn get_Channel(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Note(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Velocity(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiPolyphonicKeyPressureMessage, 527644670, 44264, 18592, 134, 142, 124, 219, 242, 15, 4, 214);
		RT_INTERFACE!{interface IMidiPolyphonicKeyPressureMessage(IMidiPolyphonicKeyPressureMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiPolyphonicKeyPressureMessage] {
			fn get_Channel(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Note(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Pressure(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiControlChangeMessage, 3085000579, 30733, 16479, 183, 129, 62, 21, 152, 201, 127, 64);
		RT_INTERFACE!{interface IMidiControlChangeMessage(IMidiControlChangeMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiControlChangeMessage] {
			fn get_Channel(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Controller(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ControlValue(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiProgramChangeMessage, 2629516408, 31294, 17191, 170, 152, 32, 184, 228, 72, 90, 248);
		RT_INTERFACE!{interface IMidiProgramChangeMessage(IMidiProgramChangeMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiProgramChangeMessage] {
			fn get_Channel(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Program(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiChannelPressureMessage, 3189745760, 25268, 19794, 163, 126, 146, 229, 77, 53, 185, 9);
		RT_INTERFACE!{interface IMidiChannelPressureMessage(IMidiChannelPressureMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiChannelPressureMessage] {
			fn get_Channel(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Pressure(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiPitchBendChangeMessage, 702500017, 11935, 20399, 140, 43, 156, 184, 42, 144, 121, 202);
		RT_INTERFACE!{interface IMidiPitchBendChangeMessage(IMidiPitchBendChangeMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiPitchBendChangeMessage] {
			fn get_Channel(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Bend(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiTimeCodeMessage, 200738941, 64099, 18972, 141, 235, 192, 232, 119, 150, 166, 215);
		RT_INTERFACE!{interface IMidiTimeCodeMessage(IMidiTimeCodeMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiTimeCodeMessage] {
			fn get_FrameType(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Values(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiSongPositionPointerMessage, 1285885014, 60510, 19172, 161, 21, 136, 220, 87, 204, 43, 121);
		RT_INTERFACE!{interface IMidiSongPositionPointerMessage(IMidiSongPositionPointerMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiSongPositionPointerMessage] {
			fn get_Beats(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiSongSelectMessage, 1240527487, 28035, 18241, 165, 191, 70, 41, 246, 190, 151, 79);
		RT_INTERFACE!{interface IMidiSongSelectMessage(IMidiSongSelectMessageVtbl): IInspectable(IInspectableVtbl) [IID_IMidiSongSelectMessage] {
			fn get_Song(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiNoteOffMessageFactory, 2796699872, 42825, 16991, 138, 244, 164, 217, 121, 204, 21, 181);
		RT_INTERFACE!{interface IMidiNoteOffMessageFactory(IMidiNoteOffMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiNoteOffMessageFactory] {
			fn CreateMidiNoteOffMessage(&mut self, channel: u8, note: u8, velocity: u8, out: *mut *mut ::rt::gen::windows::devices::midi::MidiNoteOffMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiNoteOffMessage: ::rt::gen::windows::devices::midi::IMidiNoteOffMessage);
		DEFINE_IID!(IID_IMidiNoteOnMessageFactory, 2604826784, 22977, 16910, 181, 23, 21, 161, 10, 169, 96, 107);
		RT_INTERFACE!{interface IMidiNoteOnMessageFactory(IMidiNoteOnMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiNoteOnMessageFactory] {
			fn CreateMidiNoteOnMessage(&mut self, channel: u8, note: u8, velocity: u8, out: *mut *mut ::rt::gen::windows::devices::midi::MidiNoteOnMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiNoteOnMessage: ::rt::gen::windows::devices::midi::IMidiNoteOnMessage);
		DEFINE_IID!(IID_IMidiPolyphonicKeyPressureMessageFactory, 3918481470, 50355, 19922, 145, 124, 227, 73, 129, 90, 27, 59);
		RT_INTERFACE!{interface IMidiPolyphonicKeyPressureMessageFactory(IMidiPolyphonicKeyPressureMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiPolyphonicKeyPressureMessageFactory] {
			fn CreateMidiPolyphonicKeyPressureMessage(&mut self, channel: u8, note: u8, pressure: u8, out: *mut *mut ::rt::gen::windows::devices::midi::MidiPolyphonicKeyPressureMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiPolyphonicKeyPressureMessage: ::rt::gen::windows::devices::midi::IMidiPolyphonicKeyPressureMessage);
		DEFINE_IID!(IID_IMidiControlChangeMessageFactory, 716260129, 38252, 18093, 151, 82, 248, 127, 85, 5, 47, 227);
		RT_INTERFACE!{interface IMidiControlChangeMessageFactory(IMidiControlChangeMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiControlChangeMessageFactory] {
			fn CreateMidiControlChangeMessage(&mut self, channel: u8, controller: u8, controlValue: u8, out: *mut *mut ::rt::gen::windows::devices::midi::MidiControlChangeMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiControlChangeMessage: ::rt::gen::windows::devices::midi::IMidiControlChangeMessage);
		DEFINE_IID!(IID_IMidiProgramChangeMessageFactory, 3601875847, 21067, 16644, 156, 153, 101, 114, 191, 210, 226, 97);
		RT_INTERFACE!{interface IMidiProgramChangeMessageFactory(IMidiProgramChangeMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiProgramChangeMessageFactory] {
			fn CreateMidiProgramChangeMessage(&mut self, channel: u8, program: u8, out: *mut *mut ::rt::gen::windows::devices::midi::MidiProgramChangeMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiProgramChangeMessage: ::rt::gen::windows::devices::midi::IMidiProgramChangeMessage);
		DEFINE_IID!(IID_IMidiChannelPressureMessageFactory, 1645800751, 8836, 16682, 148, 207, 16, 251, 4, 132, 44, 108);
		RT_INTERFACE!{interface IMidiChannelPressureMessageFactory(IMidiChannelPressureMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiChannelPressureMessageFactory] {
			fn CreateMidiChannelPressureMessage(&mut self, channel: u8, pressure: u8, out: *mut *mut ::rt::gen::windows::devices::midi::MidiChannelPressureMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiChannelPressureMessage: ::rt::gen::windows::devices::midi::IMidiChannelPressureMessage);
		DEFINE_IID!(IID_IMidiPitchBendChangeMessageFactory, 4126072661, 53192, 18726, 179, 14, 163, 98, 35, 147, 48, 108);
		RT_INTERFACE!{interface IMidiPitchBendChangeMessageFactory(IMidiPitchBendChangeMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiPitchBendChangeMessageFactory] {
			fn CreateMidiPitchBendChangeMessage(&mut self, channel: u8, bend: u16, out: *mut *mut ::rt::gen::windows::devices::midi::MidiPitchBendChangeMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiPitchBendChangeMessage: ::rt::gen::windows::devices::midi::IMidiPitchBendChangeMessage);
		DEFINE_IID!(IID_IMidiSystemExclusiveMessageFactory, 138273314, 15220, 17184, 155, 66, 12, 168, 84, 95, 138, 36);
		RT_INTERFACE!{interface IMidiSystemExclusiveMessageFactory(IMidiSystemExclusiveMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiSystemExclusiveMessageFactory] {
			fn CreateMidiSystemExclusiveMessage(&mut self, rawData: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::devices::midi::MidiSystemExclusiveMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiSystemExclusiveMessage: ::rt::gen::windows::devices::midi::IMidiMessage);
		DEFINE_IID!(IID_IMidiTimeCodeMessageFactory, 3945830853, 30492, 16606, 185, 97, 23, 90, 116, 137, 168, 94);
		RT_INTERFACE!{interface IMidiTimeCodeMessageFactory(IMidiTimeCodeMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiTimeCodeMessageFactory] {
			fn CreateMidiTimeCodeMessage(&mut self, frameType: u8, values: u8, out: *mut *mut ::rt::gen::windows::devices::midi::MidiTimeCodeMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiTimeCodeMessage: ::rt::gen::windows::devices::midi::IMidiTimeCodeMessage);
		DEFINE_IID!(IID_IMidiSongPositionPointerMessageFactory, 2617305494, 61707, 20458, 179, 149, 245, 214, 207, 128, 246, 78);
		RT_INTERFACE!{interface IMidiSongPositionPointerMessageFactory(IMidiSongPositionPointerMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiSongPositionPointerMessageFactory] {
			fn CreateMidiSongPositionPointerMessage(&mut self, beats: u16, out: *mut *mut ::rt::gen::windows::devices::midi::MidiSongPositionPointerMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiSongPositionPointerMessage: ::rt::gen::windows::devices::midi::IMidiSongPositionPointerMessage);
		DEFINE_IID!(IID_IMidiSongSelectMessageFactory, 2223536356, 34632, 16681, 166, 108, 160, 84, 147, 247, 93, 170);
		RT_INTERFACE!{interface IMidiSongSelectMessageFactory(IMidiSongSelectMessageFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMidiSongSelectMessageFactory] {
			fn CreateMidiSongSelectMessage(&mut self, song: u8, out: *mut *mut ::rt::gen::windows::devices::midi::MidiSongSelectMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiSongSelectMessage: ::rt::gen::windows::devices::midi::IMidiSongSelectMessage);
		RT_CLASS!(MidiTuneRequestMessage: ::rt::gen::windows::devices::midi::IMidiMessage);
		RT_CLASS!(MidiTimingClockMessage: ::rt::gen::windows::devices::midi::IMidiMessage);
		RT_CLASS!(MidiStartMessage: ::rt::gen::windows::devices::midi::IMidiMessage);
		RT_CLASS!(MidiContinueMessage: ::rt::gen::windows::devices::midi::IMidiMessage);
		RT_CLASS!(MidiStopMessage: ::rt::gen::windows::devices::midi::IMidiMessage);
		RT_CLASS!(MidiActiveSensingMessage: ::rt::gen::windows::devices::midi::IMidiMessage);
		RT_CLASS!(MidiSystemResetMessage: ::rt::gen::windows::devices::midi::IMidiMessage);
		DEFINE_IID!(IID_IMidiMessageReceivedEventArgs, 1985375830, 62248, 19281, 144, 125, 179, 168, 206, 150, 191, 128);
		RT_INTERFACE!{interface IMidiMessageReceivedEventArgs(IMidiMessageReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMidiMessageReceivedEventArgs] {
			fn get_Message(&mut self, out: *mut *mut ::rt::gen::windows::devices::midi::IMidiMessage) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiMessageReceivedEventArgs: ::rt::gen::windows::devices::midi::IMidiMessageReceivedEventArgs);
		DEFINE_IID!(IID_IMidiInPortStatics, 1153710556, 26623, 19054, 139, 172, 253, 182, 97, 12, 242, 150);
		RT_INTERFACE!{interface IMidiInPortStatics(IMidiInPortStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IMidiInPortStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::midi::MidiInPort>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiInPort: ::rt::gen::windows::devices::midi::IMidiInPort);
		DEFINE_IID!(IID_IMidiOutPortStatics, 106742761, 3976, 17547, 155, 100, 169, 88, 38, 198, 91, 143);
		RT_INTERFACE!{interface IMidiOutPortStatics(IMidiOutPortStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IMidiOutPortStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::midi::IMidiOutPort>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiOutPort: ::rt::gen::windows::devices::midi::IMidiOutPort);
		DEFINE_IID!(IID_IMidiSynthesizerStatics, 1109715624, 26153, 19819, 170, 143, 212, 82, 26, 90, 49, 206);
		RT_INTERFACE!{interface IMidiSynthesizerStatics(IMidiSynthesizerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IMidiSynthesizerStatics] {
			fn CreateAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::midi::MidiSynthesizer>) -> ::w::HRESULT,
			fn CreateFromAudioDeviceAsync(&mut self, audioDevice: *mut ::rt::gen::windows::devices::enumeration::DeviceInformation, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::midi::MidiSynthesizer>) -> ::w::HRESULT,
			fn IsSynthesizer(&mut self, midiDevice: *mut ::rt::gen::windows::devices::enumeration::DeviceInformation, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(MidiSynthesizer: ::rt::gen::windows::devices::midi::IMidiSynthesizer);
		DEFINE_IID!(IID_IMidiOutPort, 2468179359, 22434, 19002, 173, 184, 70, 64, 136, 111, 102, 147);
		RT_INTERFACE!{interface IMidiOutPort(IMidiOutPortVtbl): IInspectable(IInspectableVtbl) [IID_IMidiOutPort] {
			fn SendMessage(&mut self, midiMessage: *mut ::rt::gen::windows::devices::midi::IMidiMessage) -> ::w::HRESULT,
			fn SendBuffer(&mut self, midiData: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiInPort, 3586251227, 38682, 20143, 162, 61, 234, 25, 254, 96, 127, 249);
		RT_INTERFACE!{interface IMidiInPort(IMidiInPortVtbl): IInspectable(IInspectableVtbl) [IID_IMidiInPort] {
			fn add_MessageReceived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::midi::MidiInPort, &::rt::gen::windows::devices::midi::MidiMessageReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_MessageReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMidiSynthesizer, 4040824158, 56208, 16479, 184, 174, 33, 210, 225, 127, 46, 69);
		RT_INTERFACE!{interface IMidiSynthesizer(IMidiSynthesizerVtbl): IInspectable(IInspectableVtbl) [IID_IMidiSynthesizer] {
			fn get_AudioDevice(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT,
			fn get_Volume(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn put_Volume(&mut self, value: f64) -> ::w::HRESULT
		}}
} // Windows.Devices.Midi
pub mod perception { // Windows.Devices.Perception
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum PerceptionFrameSourceAccessStatus: i32 {
			Unspecified (PerceptionFrameSourceAccessStatus_Unspecified) = 0, Allowed (PerceptionFrameSourceAccessStatus_Allowed) = 1, DeniedByUser (PerceptionFrameSourceAccessStatus_DeniedByUser) = 2, DeniedBySystem (PerceptionFrameSourceAccessStatus_DeniedBySystem) = 3,
		}}
		RT_ENUM! { enum PerceptionFrameSourcePropertyChangeStatus: i32 {
			Unknown (PerceptionFrameSourcePropertyChangeStatus_Unknown) = 0, Accepted (PerceptionFrameSourcePropertyChangeStatus_Accepted) = 1, LostControl (PerceptionFrameSourcePropertyChangeStatus_LostControl) = 2, PropertyNotSupported (PerceptionFrameSourcePropertyChangeStatus_PropertyNotSupported) = 3, PropertyReadOnly (PerceptionFrameSourcePropertyChangeStatus_PropertyReadOnly) = 4, ValueOutOfRange (PerceptionFrameSourcePropertyChangeStatus_ValueOutOfRange) = 5,
		}}
		DEFINE_IID!(IID_IPerceptionColorFrameSourceWatcher, 2528973714, 58983, 16580, 137, 249, 20, 98, 222, 166, 169, 204);
		RT_INTERFACE!{interface IPerceptionColorFrameSourceWatcher(IPerceptionColorFrameSourceWatcherVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrameSourceWatcher] {
			fn add_SourceAdded(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSourceWatcher, &::rt::gen::windows::devices::perception::PerceptionColorFrameSourceAddedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SourceAdded(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_SourceRemoved(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSourceWatcher, &::rt::gen::windows::devices::perception::PerceptionColorFrameSourceRemovedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SourceRemoved(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Stopped(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSourceWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Stopped(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_EnumerationCompleted(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSourceWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_EnumerationCompleted(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceWatcherStatus) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionColorFrameSourceWatcher: ::rt::gen::windows::devices::perception::IPerceptionColorFrameSourceWatcher);
		RT_CLASS!(PerceptionColorFrameSourceAddedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionColorFrameSourceAddedEventArgs);
		RT_CLASS!(PerceptionColorFrameSourceRemovedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionColorFrameSourceRemovedEventArgs);
		DEFINE_IID!(IID_IPerceptionDepthFrameSourceWatcher, 2014222033, 36098, 19755, 173, 164, 91, 166, 36, 160, 235, 16);
		RT_INTERFACE!{interface IPerceptionDepthFrameSourceWatcher(IPerceptionDepthFrameSourceWatcherVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrameSourceWatcher] {
			fn add_SourceAdded(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceWatcher, &::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceAddedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SourceAdded(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_SourceRemoved(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceWatcher, &::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceRemovedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SourceRemoved(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Stopped(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Stopped(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_EnumerationCompleted(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_EnumerationCompleted(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceWatcherStatus) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionDepthFrameSourceWatcher: ::rt::gen::windows::devices::perception::IPerceptionDepthFrameSourceWatcher);
		RT_CLASS!(PerceptionDepthFrameSourceAddedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionDepthFrameSourceAddedEventArgs);
		RT_CLASS!(PerceptionDepthFrameSourceRemovedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionDepthFrameSourceRemovedEventArgs);
		DEFINE_IID!(IID_IPerceptionInfraredFrameSourceWatcher, 943521689, 55052, 17485, 168, 176, 114, 12, 46, 102, 254, 59);
		RT_INTERFACE!{interface IPerceptionInfraredFrameSourceWatcher(IPerceptionInfraredFrameSourceWatcherVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrameSourceWatcher] {
			fn add_SourceAdded(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceWatcher, &::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceAddedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SourceAdded(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_SourceRemoved(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceWatcher, &::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceRemovedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SourceRemoved(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Stopped(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Stopped(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_EnumerationCompleted(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceWatcher, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_EnumerationCompleted(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::enumeration::DeviceWatcherStatus) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionInfraredFrameSourceWatcher: ::rt::gen::windows::devices::perception::IPerceptionInfraredFrameSourceWatcher);
		RT_CLASS!(PerceptionInfraredFrameSourceAddedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionInfraredFrameSourceAddedEventArgs);
		RT_CLASS!(PerceptionInfraredFrameSourceRemovedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionInfraredFrameSourceRemovedEventArgs);
		DEFINE_IID!(IID_IPerceptionColorFrameSourceAddedEventArgs, 3513513190, 55844, 17452, 187, 213, 85, 84, 155, 91, 148, 243);
		RT_INTERFACE!{interface IPerceptionColorFrameSourceAddedEventArgs(IPerceptionColorFrameSourceAddedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrameSourceAddedEventArgs] {
			fn get_FrameSource(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionColorFrameSource) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionColorFrameSource: ::rt::gen::windows::devices::perception::IPerceptionColorFrameSource);
		DEFINE_IID!(IID_IPerceptionColorFrameSourceRemovedEventArgs, 3531078249, 60236, 17135, 186, 79, 40, 143, 97, 92, 147, 193);
		RT_INTERFACE!{interface IPerceptionColorFrameSourceRemovedEventArgs(IPerceptionColorFrameSourceRemovedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrameSourceRemovedEventArgs] {
			fn get_FrameSource(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionColorFrameSource) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionDepthFrameSourceAddedEventArgs, 2477031784, 35832, 17874, 162, 248, 74, 192, 147, 28, 199, 166);
		RT_INTERFACE!{interface IPerceptionDepthFrameSourceAddedEventArgs(IPerceptionDepthFrameSourceAddedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrameSourceAddedEventArgs] {
			fn get_FrameSource(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionDepthFrameSource: ::rt::gen::windows::devices::perception::IPerceptionDepthFrameSource);
		DEFINE_IID!(IID_IPerceptionDepthFrameSourceRemovedEventArgs, 2696989773, 59756, 19841, 134, 221, 56, 185, 94, 73, 198, 223);
		RT_INTERFACE!{interface IPerceptionDepthFrameSourceRemovedEventArgs(IPerceptionDepthFrameSourceRemovedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrameSourceRemovedEventArgs] {
			fn get_FrameSource(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionInfraredFrameSourceAddedEventArgs, 1832075552, 38350, 18016, 144, 122, 217, 128, 53, 170, 43, 124);
		RT_INTERFACE!{interface IPerceptionInfraredFrameSourceAddedEventArgs(IPerceptionInfraredFrameSourceAddedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrameSourceAddedEventArgs] {
			fn get_FrameSource(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionInfraredFrameSource: ::rt::gen::windows::devices::perception::IPerceptionInfraredFrameSource);
		DEFINE_IID!(IID_IPerceptionInfraredFrameSourceRemovedEventArgs, 3927605361, 31344, 19041, 175, 148, 7, 48, 56, 83, 246, 149);
		RT_INTERFACE!{interface IPerceptionInfraredFrameSourceRemovedEventArgs(IPerceptionInfraredFrameSourceRemovedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrameSourceRemovedEventArgs] {
			fn get_FrameSource(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKnownPerceptionFrameSourcePropertiesStatics, 1576127650, 504, 19079, 184, 89, 213, 229, 183, 225, 222, 71);
		RT_INTERFACE!{interface IKnownPerceptionFrameSourcePropertiesStatics(IKnownPerceptionFrameSourcePropertiesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownPerceptionFrameSourcePropertiesStatics] {
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_PhysicalDeviceIds(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_FrameKind(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DeviceModelVersion(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_EnclosureLocation(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKnownPerceptionFrameSourcePropertiesStatics2, 2848483441, 1500, 19021, 138, 92, 164, 236, 242, 107, 188, 70);
		RT_INTERFACE!{interface IKnownPerceptionFrameSourcePropertiesStatics2(IKnownPerceptionFrameSourcePropertiesStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IKnownPerceptionFrameSourcePropertiesStatics2] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKnownPerceptionVideoFrameSourcePropertiesStatics, 1576127650, 504, 19079, 184, 89, 213, 229, 183, 225, 222, 72);
		RT_INTERFACE!{interface IKnownPerceptionVideoFrameSourcePropertiesStatics(IKnownPerceptionVideoFrameSourcePropertiesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownPerceptionVideoFrameSourcePropertiesStatics] {
			fn get_VideoProfile(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SupportedVideoProfiles(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_AvailableVideoProfiles(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsMirrored(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_CameraIntrinsics(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKnownPerceptionInfraredFrameSourcePropertiesStatics, 1576127650, 504, 19079, 184, 89, 213, 229, 183, 225, 222, 73);
		RT_INTERFACE!{interface IKnownPerceptionInfraredFrameSourcePropertiesStatics(IKnownPerceptionInfraredFrameSourcePropertiesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownPerceptionInfraredFrameSourcePropertiesStatics] {
			fn get_Exposure(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_AutoExposureEnabled(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ExposureCompensation(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ActiveIlluminationEnabled(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_AmbientSubtractionEnabled(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_StructureLightPatternEnabled(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_InterleavedIlluminationEnabled(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKnownPerceptionDepthFrameSourcePropertiesStatics, 1576127650, 504, 19079, 184, 89, 213, 229, 183, 225, 222, 74);
		RT_INTERFACE!{interface IKnownPerceptionDepthFrameSourcePropertiesStatics(IKnownPerceptionDepthFrameSourcePropertiesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownPerceptionDepthFrameSourcePropertiesStatics] {
			fn get_MinDepth(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_MaxDepth(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKnownPerceptionColorFrameSourcePropertiesStatics, 1576127650, 504, 19079, 184, 89, 213, 229, 183, 225, 222, 75);
		RT_INTERFACE!{interface IKnownPerceptionColorFrameSourcePropertiesStatics(IKnownPerceptionColorFrameSourcePropertiesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownPerceptionColorFrameSourcePropertiesStatics] {
			fn get_Exposure(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_AutoExposureEnabled(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ExposureCompensation(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKnownPerceptionVideoProfilePropertiesStatics, 2399724263, 23158, 17379, 161, 58, 218, 61, 145, 169, 239, 152);
		RT_INTERFACE!{interface IKnownPerceptionVideoProfilePropertiesStatics(IKnownPerceptionVideoProfilePropertiesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownPerceptionVideoProfilePropertiesStatics] {
			fn get_BitmapPixelFormat(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_BitmapAlphaMode(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Width(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Height(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_FrameDuration(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKnownCameraIntrinsicsPropertiesStatics, 146815352, 17274, 19863, 166, 99, 253, 49, 149, 96, 2, 73);
		RT_INTERFACE!{interface IKnownCameraIntrinsicsPropertiesStatics(IKnownCameraIntrinsicsPropertiesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownCameraIntrinsicsPropertiesStatics] {
			fn get_FocalLength(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_PrincipalPoint(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_RadialDistortion(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_TangentialDistortion(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionFrameSourcePropertyChangeResult, 506673418, 15504, 19746, 184, 152, 244, 43, 186, 100, 24, 255);
		RT_INTERFACE!{interface IPerceptionFrameSourcePropertyChangeResult(IPerceptionFrameSourcePropertyChangeResultVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFrameSourcePropertyChangeResult] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeStatus) -> ::w::HRESULT,
			fn get_NewValue(&mut self, out: *mut *mut IInspectable) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionControlSession: ::rt::gen::windows::devices::perception::IPerceptionControlSession);
		RT_CLASS!(PerceptionFrameSourcePropertyChangeResult: ::rt::gen::windows::devices::perception::IPerceptionFrameSourcePropertyChangeResult);
		DEFINE_IID!(IID_IPerceptionFrameSourcePropertiesChangedEventArgs, 1818812520, 48369, 20172, 184, 145, 118, 37, 209, 36, 75, 107);
		RT_INTERFACE!{interface IPerceptionFrameSourcePropertiesChangedEventArgs(IPerceptionFrameSourcePropertiesChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFrameSourcePropertiesChangedEventArgs] {
			fn get_CollectionChange(&mut self, out: *mut ::rt::gen::windows::foundation::collections::CollectionChange) -> ::w::HRESULT,
			fn get_Key(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionInfraredFrameSourceStatics, 1576258722, 504, 19079, 184, 89, 213, 229, 183, 225, 222, 71);
		RT_INTERFACE!{interface IPerceptionInfraredFrameSourceStatics(IPerceptionInfraredFrameSourceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrameSourceStatics] {
			fn CreateWatcher(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceWatcher) -> ::w::HRESULT,
			fn FindAllAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource>>) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, id: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource>) -> ::w::HRESULT,
			fn RequestAccessAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::perception::PerceptionFrameSourceAccessStatus>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionDepthFrameSourceStatics, 1576258722, 504, 19079, 184, 89, 213, 229, 183, 225, 222, 72);
		RT_INTERFACE!{interface IPerceptionDepthFrameSourceStatics(IPerceptionDepthFrameSourceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrameSourceStatics] {
			fn CreateWatcher(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceWatcher) -> ::w::HRESULT,
			fn FindAllAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSource>>) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, id: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSource>) -> ::w::HRESULT,
			fn RequestAccessAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::perception::PerceptionFrameSourceAccessStatus>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionColorFrameSourceStatics, 1576258722, 504, 19079, 184, 89, 213, 229, 183, 225, 222, 73);
		RT_INTERFACE!{interface IPerceptionColorFrameSourceStatics(IPerceptionColorFrameSourceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrameSourceStatics] {
			fn CreateWatcher(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionColorFrameSourceWatcher) -> ::w::HRESULT,
			fn FindAllAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionColorFrameSource>>) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, id: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionColorFrameSource>) -> ::w::HRESULT,
			fn RequestAccessAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::perception::PerceptionFrameSourceAccessStatus>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionColorFrameSource, 3698178684, 2904, 18061, 156, 161, 109, 176, 76, 192, 71, 124);
		RT_INTERFACE!{interface IPerceptionColorFrameSource(IPerceptionColorFrameSourceVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrameSource] {
			fn add_AvailableChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AvailableChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ActiveChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ActiveChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_PropertiesChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSource, &::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertiesChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_PropertiesChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_VideoProfileChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_VideoProfileChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_CameraIntrinsicsChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_CameraIntrinsicsChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DisplayName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DeviceKind(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Available(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Active(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsControlled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT,
			fn get_SupportedVideoProfiles(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionVideoProfile>) -> ::w::HRESULT,
			fn get_AvailableVideoProfiles(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionVideoProfile>) -> ::w::HRESULT,
			fn get_VideoProfile(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionVideoProfile) -> ::w::HRESULT,
			fn get_CameraIntrinsics(&mut self, out: *mut *mut ::rt::gen::windows::media::devices::core::CameraIntrinsics) -> ::w::HRESULT,
			fn AcquireControlSession(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionControlSession) -> ::w::HRESULT,
			fn CanControlIndependentlyFrom(&mut self, targetId: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsCorrelatedWith(&mut self, targetId: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn TryGetTransformTo(&mut self, targetId: ::w::HSTRING, result: *mut ::rt::gen::windows::foundation::numerics::Matrix4x4, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn TryGetDepthCorrelatedCameraIntrinsicsAsync(&mut self, correlatedDepthFrameSource: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCameraIntrinsics>) -> ::w::HRESULT,
			fn TryGetDepthCorrelatedCoordinateMapperAsync(&mut self, targetSourceId: ::w::HSTRING, correlatedDepthFrameSource: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCoordinateMapper>) -> ::w::HRESULT,
			fn TrySetVideoProfileAsync(&mut self, controlSession: *mut ::rt::gen::windows::devices::perception::PerceptionControlSession, profile: *mut ::rt::gen::windows::devices::perception::PerceptionVideoProfile, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeResult>) -> ::w::HRESULT,
			fn OpenReader(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionColorFrameReader) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionFrameSourcePropertiesChangedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionFrameSourcePropertiesChangedEventArgs);
		RT_CLASS!(PerceptionVideoProfile: ::rt::gen::windows::devices::perception::IPerceptionVideoProfile);
		RT_CLASS!(PerceptionDepthCorrelatedCameraIntrinsics: ::rt::gen::windows::devices::perception::IPerceptionDepthCorrelatedCameraIntrinsics);
		RT_CLASS!(PerceptionDepthCorrelatedCoordinateMapper: ::rt::gen::windows::devices::perception::IPerceptionDepthCorrelatedCoordinateMapper);
		RT_CLASS!(PerceptionColorFrameReader: ::rt::gen::windows::devices::perception::IPerceptionColorFrameReader);
		DEFINE_IID!(IID_IPerceptionColorFrameSource2, 4169140453, 22065, 17901, 173, 152, 140, 106, 160, 76, 251, 145);
		RT_INTERFACE!{interface IPerceptionColorFrameSource2(IPerceptionColorFrameSource2Vtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrameSource2] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionDepthFrameSource, 2043950038, 18427, 19953, 191, 201, 240, 29, 64, 189, 153, 66);
		RT_INTERFACE!{interface IPerceptionDepthFrameSource(IPerceptionDepthFrameSourceVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrameSource] {
			fn add_AvailableChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AvailableChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ActiveChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ActiveChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_PropertiesChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, &::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertiesChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_PropertiesChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_VideoProfileChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_VideoProfileChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_CameraIntrinsicsChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_CameraIntrinsicsChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DisplayName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DeviceKind(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Available(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Active(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsControlled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT,
			fn get_SupportedVideoProfiles(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionVideoProfile>) -> ::w::HRESULT,
			fn get_AvailableVideoProfiles(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionVideoProfile>) -> ::w::HRESULT,
			fn get_VideoProfile(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionVideoProfile) -> ::w::HRESULT,
			fn get_CameraIntrinsics(&mut self, out: *mut *mut ::rt::gen::windows::media::devices::core::CameraIntrinsics) -> ::w::HRESULT,
			fn AcquireControlSession(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionControlSession) -> ::w::HRESULT,
			fn CanControlIndependentlyFrom(&mut self, targetId: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsCorrelatedWith(&mut self, targetId: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn TryGetTransformTo(&mut self, targetId: ::w::HSTRING, result: *mut ::rt::gen::windows::foundation::numerics::Matrix4x4, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn TryGetDepthCorrelatedCameraIntrinsicsAsync(&mut self, target: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCameraIntrinsics>) -> ::w::HRESULT,
			fn TryGetDepthCorrelatedCoordinateMapperAsync(&mut self, targetId: ::w::HSTRING, depthFrameSourceToMapWith: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCoordinateMapper>) -> ::w::HRESULT,
			fn TrySetVideoProfileAsync(&mut self, controlSession: *mut ::rt::gen::windows::devices::perception::PerceptionControlSession, profile: *mut ::rt::gen::windows::devices::perception::PerceptionVideoProfile, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeResult>) -> ::w::HRESULT,
			fn OpenReader(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameReader) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionDepthFrameReader: ::rt::gen::windows::devices::perception::IPerceptionDepthFrameReader);
		DEFINE_IID!(IID_IPerceptionDepthFrameSource2, 3822206254, 28204, 20077, 145, 217, 112, 76, 216, 223, 247, 157);
		RT_INTERFACE!{interface IPerceptionDepthFrameSource2(IPerceptionDepthFrameSource2Vtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrameSource2] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionInfraredFrameSource, 1437632322, 6152, 18766, 158, 48, 157, 42, 123, 232, 247, 0);
		RT_INTERFACE!{interface IPerceptionInfraredFrameSource(IPerceptionInfraredFrameSourceVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrameSource] {
			fn add_AvailableChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AvailableChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ActiveChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ActiveChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_PropertiesChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource, &::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertiesChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_PropertiesChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_VideoProfileChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_VideoProfileChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_CameraIntrinsicsChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_CameraIntrinsicsChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DisplayName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DeviceKind(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Available(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Active(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsControlled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT,
			fn get_SupportedVideoProfiles(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionVideoProfile>) -> ::w::HRESULT,
			fn get_AvailableVideoProfiles(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::PerceptionVideoProfile>) -> ::w::HRESULT,
			fn get_VideoProfile(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionVideoProfile) -> ::w::HRESULT,
			fn get_CameraIntrinsics(&mut self, out: *mut *mut ::rt::gen::windows::media::devices::core::CameraIntrinsics) -> ::w::HRESULT,
			fn AcquireControlSession(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionControlSession) -> ::w::HRESULT,
			fn CanControlIndependentlyFrom(&mut self, targetId: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsCorrelatedWith(&mut self, targetId: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn TryGetTransformTo(&mut self, targetId: ::w::HSTRING, result: *mut ::rt::gen::windows::foundation::numerics::Matrix4x4, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn TryGetDepthCorrelatedCameraIntrinsicsAsync(&mut self, target: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCameraIntrinsics>) -> ::w::HRESULT,
			fn TryGetDepthCorrelatedCoordinateMapperAsync(&mut self, targetId: ::w::HSTRING, depthFrameSourceToMapWith: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCoordinateMapper>) -> ::w::HRESULT,
			fn TrySetVideoProfileAsync(&mut self, controlSession: *mut ::rt::gen::windows::devices::perception::PerceptionControlSession, profile: *mut ::rt::gen::windows::devices::perception::PerceptionVideoProfile, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeResult>) -> ::w::HRESULT,
			fn OpenReader(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionInfraredFrameReader) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionInfraredFrameReader: ::rt::gen::windows::devices::perception::IPerceptionInfraredFrameReader);
		DEFINE_IID!(IID_IPerceptionInfraredFrameSource2, 3704936344, 19211, 17152, 141, 133, 65, 8, 23, 250, 160, 50);
		RT_INTERFACE!{interface IPerceptionInfraredFrameSource2(IPerceptionInfraredFrameSource2Vtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrameSource2] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionVideoProfile, 1970683555, 282, 18190, 130, 37, 111, 5, 173, 226, 86, 72);
		RT_INTERFACE!{interface IPerceptionVideoProfile(IPerceptionVideoProfileVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionVideoProfile] {
			fn get_BitmapPixelFormat(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapPixelFormat) -> ::w::HRESULT,
			fn get_BitmapAlphaMode(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapAlphaMode) -> ::w::HRESULT,
			fn get_Width(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Height(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_FrameDuration(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn IsEqual(&mut self, other: *mut ::rt::gen::windows::devices::perception::PerceptionVideoProfile, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionColorFrameArrivedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionColorFrameArrivedEventArgs);
		RT_CLASS!(PerceptionColorFrame: ::rt::gen::windows::devices::perception::IPerceptionColorFrame);
		RT_CLASS!(PerceptionDepthFrameArrivedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionDepthFrameArrivedEventArgs);
		RT_CLASS!(PerceptionDepthFrame: ::rt::gen::windows::devices::perception::IPerceptionDepthFrame);
		RT_CLASS!(PerceptionInfraredFrameArrivedEventArgs: ::rt::gen::windows::devices::perception::IPerceptionInfraredFrameArrivedEventArgs);
		RT_CLASS!(PerceptionInfraredFrame: ::rt::gen::windows::devices::perception::IPerceptionInfraredFrame);
		DEFINE_IID!(IID_IPerceptionColorFrameArrivedEventArgs, 2410480341, 34551, 19853, 185, 102, 90, 55, 97, 186, 159, 89);
		RT_INTERFACE!{interface IPerceptionColorFrameArrivedEventArgs(IPerceptionColorFrameArrivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrameArrivedEventArgs] {
			fn get_RelativeTime(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn TryOpenFrame(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionColorFrame) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionDepthFrameArrivedEventArgs, 1144858034, 45698, 17975, 145, 115, 172, 151, 132, 53, 201, 133);
		RT_INTERFACE!{interface IPerceptionDepthFrameArrivedEventArgs(IPerceptionDepthFrameArrivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrameArrivedEventArgs] {
			fn get_RelativeTime(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn TryOpenFrame(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionInfraredFrameArrivedEventArgs, 2675440327, 46269, 18519, 157, 80, 190, 142, 240, 117, 218, 239);
		RT_INTERFACE!{interface IPerceptionInfraredFrameArrivedEventArgs(IPerceptionInfraredFrameArrivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrameArrivedEventArgs] {
			fn get_RelativeTime(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn TryOpenFrame(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionInfraredFrame) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionDepthCorrelatedCameraIntrinsics, 1699269121, 34526, 23521, 101, 130, 128, 127, 207, 76, 149, 207);
		RT_INTERFACE!{interface IPerceptionDepthCorrelatedCameraIntrinsics(IPerceptionDepthCorrelatedCameraIntrinsicsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthCorrelatedCameraIntrinsics] {
			fn UnprojectPixelAtCorrelatedDepth(&mut self, pixelCoordinate: ::rt::gen::windows::foundation::Point, depthFrame: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame, out: *mut ::rt::gen::windows::foundation::numerics::Vector3) -> ::w::HRESULT,
			fn UnprojectPixelsAtCorrelatedDepth(&mut self, sourceCoordinates: *mut ::rt::gen::windows::foundation::Point, depthFrame: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame, results: *mut ::rt::gen::windows::foundation::numerics::Vector3) -> ::w::HRESULT,
			fn UnprojectRegionPixelsAtCorrelatedDepthAsync(&mut self, region: ::rt::gen::windows::foundation::Rect, depthFrame: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame, results: *mut ::rt::gen::windows::foundation::numerics::Vector3, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn UnprojectAllPixelsAtCorrelatedDepthAsync(&mut self, depthFrame: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame, results: *mut ::rt::gen::windows::foundation::numerics::Vector3, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionDepthCorrelatedCoordinateMapper, 1531813149, 46582, 18076, 184, 194, 185, 122, 69, 230, 134, 59);
		RT_INTERFACE!{interface IPerceptionDepthCorrelatedCoordinateMapper(IPerceptionDepthCorrelatedCoordinateMapperVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthCorrelatedCoordinateMapper] {
			fn MapPixelToTarget(&mut self, sourcePixelCoordinate: ::rt::gen::windows::foundation::Point, depthFrame: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame, out: *mut ::rt::gen::windows::foundation::Point) -> ::w::HRESULT,
			fn MapPixelsToTarget(&mut self, sourceCoordinates: *mut ::rt::gen::windows::foundation::Point, depthFrame: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame, results: *mut ::rt::gen::windows::foundation::Point) -> ::w::HRESULT,
			fn MapRegionOfPixelsToTargetAsync(&mut self, region: ::rt::gen::windows::foundation::Rect, depthFrame: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame, targetCoordinates: *mut ::rt::gen::windows::foundation::Point, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn MapAllPixelsToTargetAsync(&mut self, depthFrame: *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame, targetCoordinates: *mut ::rt::gen::windows::foundation::Point, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionControlSession, 2576975443, 23101, 16767, 146, 57, 241, 136, 158, 84, 139, 72);
		RT_INTERFACE!{interface IPerceptionControlSession(IPerceptionControlSessionVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionControlSession] {
			fn add_ControlLost(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionControlSession, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ControlLost(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn TrySetPropertyAsync(&mut self, name: ::w::HSTRING, value: *mut IInspectable, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeResult>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionColorFrameReader, 1985017198, 47605, 17947, 131, 173, 242, 34, 175, 42, 170, 220);
		RT_INTERFACE!{interface IPerceptionColorFrameReader(IPerceptionColorFrameReaderVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrameReader] {
			fn add_FrameArrived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionColorFrameReader, &::rt::gen::windows::devices::perception::PerceptionColorFrameArrivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_FrameArrived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Source(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionColorFrameSource) -> ::w::HRESULT,
			fn get_IsPaused(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsPaused(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn TryReadLatestFrame(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionColorFrame) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionDepthFrameReader, 2980298911, 10651, 17938, 164, 247, 39, 15, 37, 160, 150, 236);
		RT_INTERFACE!{interface IPerceptionDepthFrameReader(IPerceptionDepthFrameReaderVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrameReader] {
			fn add_FrameArrived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionDepthFrameReader, &::rt::gen::windows::devices::perception::PerceptionDepthFrameArrivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_FrameArrived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Source(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource) -> ::w::HRESULT,
			fn get_IsPaused(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsPaused(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn TryReadLatestFrame(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionDepthFrame) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionInfraredFrameReader, 2036387352, 54171, 20424, 160, 74, 146, 151, 52, 198, 117, 108);
		RT_INTERFACE!{interface IPerceptionInfraredFrameReader(IPerceptionInfraredFrameReaderVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrameReader] {
			fn add_FrameArrived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::perception::PerceptionInfraredFrameReader, &::rt::gen::windows::devices::perception::PerceptionInfraredFrameArrivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_FrameArrived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_Source(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource) -> ::w::HRESULT,
			fn get_IsPaused(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsPaused(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn TryReadLatestFrame(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::PerceptionInfraredFrame) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionColorFrame, 4267840841, 11455, 20372, 152, 97, 248, 23, 234, 49, 119, 71);
		RT_INTERFACE!{interface IPerceptionColorFrame(IPerceptionColorFrameVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionColorFrame] {
			fn get_VideoFrame(&mut self, out: *mut *mut ::rt::gen::windows::media::VideoFrame) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionDepthFrame, 2742780412, 39174, 20477, 145, 97, 0, 36, 179, 96, 182, 87);
		RT_INTERFACE!{interface IPerceptionDepthFrame(IPerceptionDepthFrameVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionDepthFrame] {
			fn get_VideoFrame(&mut self, out: *mut *mut ::rt::gen::windows::media::VideoFrame) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionInfraredFrame, 2961728118, 33950, 19578, 138, 230, 181, 96, 100, 83, 33, 83);
		RT_INTERFACE!{interface IPerceptionInfraredFrame(IPerceptionInfraredFrameVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionInfraredFrame] {
			fn get_VideoFrame(&mut self, out: *mut *mut ::rt::gen::windows::media::VideoFrame) -> ::w::HRESULT
		}}
pub mod provider { // Windows.Devices.Perception.Provider
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_IKnownPerceptionFrameKindStatics, 988172758, 38505, 16646, 159, 174, 72, 53, 193, 185, 97, 4);
		RT_INTERFACE!{interface IKnownPerceptionFrameKindStatics(IKnownPerceptionFrameKindStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownPerceptionFrameKindStatics] {
			fn get_Color(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Depth(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Infrared(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionFrameProviderManagerServiceStatics, 2927855334, 51929, 17241, 143, 150, 142, 174, 81, 129, 5, 38);
		RT_INTERFACE!{interface IPerceptionFrameProviderManagerServiceStatics(IPerceptionFrameProviderManagerServiceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFrameProviderManagerServiceStatics] {
			fn RegisterFrameProviderInfo(&mut self, manager: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderManager, frameProviderInfo: *mut ::rt::gen::windows::devices::perception::provider::PerceptionFrameProviderInfo) -> ::w::HRESULT,
			fn UnregisterFrameProviderInfo(&mut self, manager: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderManager, frameProviderInfo: *mut ::rt::gen::windows::devices::perception::provider::PerceptionFrameProviderInfo) -> ::w::HRESULT,
			fn RegisterFaceAuthenticationGroup(&mut self, manager: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderManager, faceAuthenticationGroup: *mut ::rt::gen::windows::devices::perception::provider::PerceptionFaceAuthenticationGroup) -> ::w::HRESULT,
			fn UnregisterFaceAuthenticationGroup(&mut self, manager: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderManager, faceAuthenticationGroup: *mut ::rt::gen::windows::devices::perception::provider::PerceptionFaceAuthenticationGroup) -> ::w::HRESULT,
			fn RegisterControlGroup(&mut self, manager: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderManager, controlGroup: *mut ::rt::gen::windows::devices::perception::provider::PerceptionControlGroup) -> ::w::HRESULT,
			fn UnregisterControlGroup(&mut self, manager: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderManager, controlGroup: *mut ::rt::gen::windows::devices::perception::provider::PerceptionControlGroup) -> ::w::HRESULT,
			fn RegisterCorrelationGroup(&mut self, manager: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderManager, correlationGroup: *mut ::rt::gen::windows::devices::perception::provider::PerceptionCorrelationGroup) -> ::w::HRESULT,
			fn UnregisterCorrelationGroup(&mut self, manager: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderManager, correlationGroup: *mut ::rt::gen::windows::devices::perception::provider::PerceptionCorrelationGroup) -> ::w::HRESULT,
			fn UpdateAvailabilityForProvider(&mut self, provider: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProvider, available: ::w::BOOL) -> ::w::HRESULT,
			fn PublishFrameForProvider(&mut self, provider: *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProvider, frame: *mut ::rt::gen::windows::devices::perception::provider::PerceptionFrame) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionFrameProviderInfo: ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProviderInfo);
		RT_CLASS!(PerceptionPropertyChangeRequest: ::rt::gen::windows::devices::perception::provider::IPerceptionPropertyChangeRequest);
		RT_CLASS!(PerceptionFaceAuthenticationGroup: ::rt::gen::windows::devices::perception::provider::IPerceptionFaceAuthenticationGroup);
		RT_CLASS!(PerceptionControlGroup: ::rt::gen::windows::devices::perception::provider::IPerceptionControlGroup);
		RT_CLASS!(PerceptionCorrelationGroup: ::rt::gen::windows::devices::perception::provider::IPerceptionCorrelationGroup);
		RT_CLASS!(PerceptionFrame: ::rt::gen::windows::devices::perception::provider::IPerceptionFrame);
		DEFINE_IID!(IID_IPerceptionFrameProviderInfo, 3433650664, 31102, 20099, 155, 135, 3, 106, 116, 20, 47, 196);
		RT_INTERFACE!{interface IPerceptionFrameProviderInfo(IPerceptionFrameProviderInfoVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFrameProviderInfo] {
			fn get_Id(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_Id(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_DisplayName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_DisplayName(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_DeviceKind(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_DeviceKind(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_FrameKind(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_FrameKind(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_Hidden(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_Hidden(&mut self, value: ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionControlGroupFactory, 790295264, 47857, 17723, 190, 212, 205, 157, 70, 25, 21, 76);
		RT_INTERFACE!{interface IPerceptionControlGroupFactory(IPerceptionControlGroupFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionControlGroupFactory] {
			fn Create(&mut self, ids: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::devices::perception::provider::PerceptionControlGroup) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionControlGroup, 388778114, 12249, 19534, 186, 52, 253, 242, 10, 115, 221, 229);
		RT_INTERFACE!{interface IPerceptionControlGroup(IPerceptionControlGroupVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionControlGroup] {
			fn get_FrameProviderIds(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&str>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_PerceptionStartFaceAuthenticationHandler, 1954639146, 8336, 18032, 140, 72, 239, 57, 231, 255, 124, 38);
		RT_DELEGATE!{delegate PerceptionStartFaceAuthenticationHandler(PerceptionStartFaceAuthenticationHandlerVtbl, PerceptionStartFaceAuthenticationHandlerImpl) [IID_PerceptionStartFaceAuthenticationHandler] {
			fn Invoke(&mut self, sender: *mut ::rt::gen::windows::devices::perception::provider::PerceptionFaceAuthenticationGroup, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_PerceptionStopFaceAuthenticationHandler, 947840682, 35277, 18462, 170, 222, 221, 146, 247, 11, 42, 215);
		RT_DELEGATE!{delegate PerceptionStopFaceAuthenticationHandler(PerceptionStopFaceAuthenticationHandlerVtbl, PerceptionStopFaceAuthenticationHandlerImpl) [IID_PerceptionStopFaceAuthenticationHandler] {
			fn Invoke(&mut self, sender: *mut ::rt::gen::windows::devices::perception::provider::PerceptionFaceAuthenticationGroup) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionFaceAuthenticationGroupFactory, 3867805140, 46604, 16628, 188, 185, 242, 77, 70, 70, 115, 32);
		RT_INTERFACE!{interface IPerceptionFaceAuthenticationGroupFactory(IPerceptionFaceAuthenticationGroupFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFaceAuthenticationGroupFactory] {
			fn Create(&mut self, ids: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, startHandler: *mut ::rt::gen::windows::devices::perception::provider::PerceptionStartFaceAuthenticationHandler, stopHandler: *mut ::rt::gen::windows::devices::perception::provider::PerceptionStopFaceAuthenticationHandler, out: *mut *mut ::rt::gen::windows::devices::perception::provider::PerceptionFaceAuthenticationGroup) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionFaceAuthenticationGroup, 3892418580, 19089, 16816, 131, 166, 136, 26, 23, 117, 53, 62);
		RT_INTERFACE!{interface IPerceptionFaceAuthenticationGroup(IPerceptionFaceAuthenticationGroupVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFaceAuthenticationGroup] {
			fn get_FrameProviderIds(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&str>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionCorrelationFactory, 3567698981, 10372, 19087, 129, 52, 40, 53, 215, 40, 108, 191);
		RT_INTERFACE!{interface IPerceptionCorrelationFactory(IPerceptionCorrelationFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionCorrelationFactory] {
			fn Create(&mut self, targetId: ::w::HSTRING, position: ::rt::gen::windows::foundation::numerics::Vector3, orientation: ::rt::gen::windows::foundation::numerics::Quaternion, out: *mut *mut ::rt::gen::windows::devices::perception::provider::PerceptionCorrelation) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionCorrelation: ::rt::gen::windows::devices::perception::provider::IPerceptionCorrelation);
		DEFINE_IID!(IID_IPerceptionCorrelation, 3021150850, 57333, 16455, 138, 25, 59, 77, 128, 95, 113, 118);
		RT_INTERFACE!{interface IPerceptionCorrelation(IPerceptionCorrelationVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionCorrelation] {
			fn get_TargetId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Position(&mut self, out: *mut ::rt::gen::windows::foundation::numerics::Vector3) -> ::w::HRESULT,
			fn get_Orientation(&mut self, out: *mut ::rt::gen::windows::foundation::numerics::Quaternion) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionCorrelationGroupFactory, 2113806472, 25567, 18669, 131, 177, 74, 184, 41, 19, 41, 149);
		RT_INTERFACE!{interface IPerceptionCorrelationGroupFactory(IPerceptionCorrelationGroupFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionCorrelationGroupFactory] {
			fn Create(&mut self, relativeLocations: *mut ::rt::gen::windows::foundation::collections::IIterable<&::rt::gen::windows::devices::perception::provider::PerceptionCorrelation>, out: *mut *mut ::rt::gen::windows::devices::perception::provider::PerceptionCorrelationGroup) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionCorrelationGroup, 1965689094, 13991, 18363, 155, 121, 86, 204, 107, 116, 103, 112);
		RT_INTERFACE!{interface IPerceptionCorrelationGroup(IPerceptionCorrelationGroupVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionCorrelationGroup] {
			fn get_RelativeLocations(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::perception::provider::PerceptionCorrelation>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionFrame, 2097051685, 21691, 19869, 190, 197, 142, 246, 97, 81, 210, 172);
		RT_INTERFACE!{interface IPerceptionFrame(IPerceptionFrameVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFrame] {
			fn get_RelativeTime(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn put_RelativeTime(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::ValueSet) -> ::w::HRESULT,
			fn get_FrameData(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IMemoryBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionVideoFrameAllocatorFactory, 442020065, 59674, 18462, 184, 118, 168, 158, 43, 188, 107, 51);
		RT_INTERFACE!{interface IPerceptionVideoFrameAllocatorFactory(IPerceptionVideoFrameAllocatorFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionVideoFrameAllocatorFactory] {
			fn Create(&mut self, maxOutstandingFrameCountForWrite: u32, format: ::rt::gen::windows::graphics::imaging::BitmapPixelFormat, resolution: ::rt::gen::windows::foundation::Size, alpha: ::rt::gen::windows::graphics::imaging::BitmapAlphaMode, out: *mut *mut ::rt::gen::windows::devices::perception::provider::PerceptionVideoFrameAllocator) -> ::w::HRESULT
		}}
		RT_CLASS!(PerceptionVideoFrameAllocator: ::rt::gen::windows::devices::perception::provider::IPerceptionVideoFrameAllocator);
		DEFINE_IID!(IID_IPerceptionPropertyChangeRequest, 1012591441, 13579, 19960, 148, 20, 89, 224, 152, 21, 81, 11);
		RT_INTERFACE!{interface IPerceptionPropertyChangeRequest(IPerceptionPropertyChangeRequestVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionPropertyChangeRequest] {
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Value(&mut self, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeStatus) -> ::w::HRESULT,
			fn put_Status(&mut self, value: ::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeStatus) -> ::w::HRESULT,
			fn GetDeferral(&mut self, out: *mut *mut ::rt::gen::windows::foundation::Deferral) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionFrameProviderManager, 2841234951, 60115, 13279, 142, 193, 185, 36, 171, 224, 25, 196);
		RT_INTERFACE!{interface IPerceptionFrameProviderManager(IPerceptionFrameProviderManagerVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFrameProviderManager] {
			fn GetFrameProvider(&mut self, frameProviderInfo: *mut ::rt::gen::windows::devices::perception::provider::PerceptionFrameProviderInfo, out: *mut *mut ::rt::gen::windows::devices::perception::provider::IPerceptionFrameProvider) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionFrameProvider, 2035251897, 45949, 15155, 161, 13, 48, 98, 100, 25, 206, 101);
		RT_INTERFACE!{interface IPerceptionFrameProvider(IPerceptionFrameProviderVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionFrameProvider] {
			fn get_FrameProviderInfo(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::provider::PerceptionFrameProviderInfo) -> ::w::HRESULT,
			fn get_Available(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IPropertySet) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT,
			fn SetProperty(&mut self, value: *mut ::rt::gen::windows::devices::perception::provider::PerceptionPropertyChangeRequest) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPerceptionVideoFrameAllocator, 1278781402, 64984, 20180, 160, 57, 42, 111, 155, 35, 80, 56);
		RT_INTERFACE!{interface IPerceptionVideoFrameAllocator(IPerceptionVideoFrameAllocatorVtbl): IInspectable(IInspectableVtbl) [IID_IPerceptionVideoFrameAllocator] {
			fn AllocateFrame(&mut self, out: *mut *mut ::rt::gen::windows::devices::perception::provider::PerceptionFrame) -> ::w::HRESULT,
			fn CopyFromVideoFrame(&mut self, frame: *mut ::rt::gen::windows::media::VideoFrame, out: *mut *mut ::rt::gen::windows::devices::perception::provider::PerceptionFrame) -> ::w::HRESULT
		}}
} // Windows.Devices.Perception.Provider
} // Windows.Devices.Perception
pub mod pointofservice { // Windows.Devices.PointOfService
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum UnifiedPosErrorSeverity: i32 {
			UnknownErrorSeverity (UnifiedPosErrorSeverity_UnknownErrorSeverity) = 0, Warning (UnifiedPosErrorSeverity_Warning) = 1, Recoverable (UnifiedPosErrorSeverity_Recoverable) = 2, Unrecoverable (UnifiedPosErrorSeverity_Unrecoverable) = 3, AssistanceRequired (UnifiedPosErrorSeverity_AssistanceRequired) = 4, Fatal (UnifiedPosErrorSeverity_Fatal) = 5,
		}}
		RT_ENUM! { enum UnifiedPosPowerReportingType: i32 {
			UnknownPowerReportingType (UnifiedPosPowerReportingType_UnknownPowerReportingType) = 0, Standard (UnifiedPosPowerReportingType_Standard) = 1, Advanced (UnifiedPosPowerReportingType_Advanced) = 2,
		}}
		RT_ENUM! { enum UnifiedPosHealthCheckLevel: i32 {
			UnknownHealthCheckLevel (UnifiedPosHealthCheckLevel_UnknownHealthCheckLevel) = 0, POSInternal (UnifiedPosHealthCheckLevel_POSInternal) = 1, External (UnifiedPosHealthCheckLevel_External) = 2, Interactive (UnifiedPosHealthCheckLevel_Interactive) = 3,
		}}
		RT_ENUM! { enum UnifiedPosErrorReason: i32 {
			UnknownErrorReason (UnifiedPosErrorReason_UnknownErrorReason) = 0, NoService (UnifiedPosErrorReason_NoService) = 1, Disabled (UnifiedPosErrorReason_Disabled) = 2, Illegal (UnifiedPosErrorReason_Illegal) = 3, NoHardware (UnifiedPosErrorReason_NoHardware) = 4, Closed (UnifiedPosErrorReason_Closed) = 5, Offline (UnifiedPosErrorReason_Offline) = 6, Failure (UnifiedPosErrorReason_Failure) = 7, Timeout (UnifiedPosErrorReason_Timeout) = 8, Busy (UnifiedPosErrorReason_Busy) = 9, Extended (UnifiedPosErrorReason_Extended) = 10,
		}}
		RT_ENUM! { enum MagneticStripeReaderAuthenticationLevel: i32 {
			NotSupported (MagneticStripeReaderAuthenticationLevel_NotSupported) = 0, Optional (MagneticStripeReaderAuthenticationLevel_Optional) = 1, Required (MagneticStripeReaderAuthenticationLevel_Required) = 2,
		}}
		RT_ENUM! { enum PosPrinterMapMode: i32 {
			Dots (PosPrinterMapMode_Dots) = 0, Twips (PosPrinterMapMode_Twips) = 1, English (PosPrinterMapMode_English) = 2, Metric (PosPrinterMapMode_Metric) = 3,
		}}
		RT_ENUM! { enum PosPrinterCartridgeSensors: u32 {
			None (PosPrinterCartridgeSensors_None) = 0, Removed (PosPrinterCartridgeSensors_Removed) = 1, Empty (PosPrinterCartridgeSensors_Empty) = 2, HeadCleaning (PosPrinterCartridgeSensors_HeadCleaning) = 4, NearEnd (PosPrinterCartridgeSensors_NearEnd) = 8,
		}}
		RT_ENUM! { enum PosPrinterColorCapabilities: u32 {
			None (PosPrinterColorCapabilities_None) = 0, Primary (PosPrinterColorCapabilities_Primary) = 1, Custom1 (PosPrinterColorCapabilities_Custom1) = 2, Custom2 (PosPrinterColorCapabilities_Custom2) = 4, Custom3 (PosPrinterColorCapabilities_Custom3) = 8, Custom4 (PosPrinterColorCapabilities_Custom4) = 16, Custom5 (PosPrinterColorCapabilities_Custom5) = 32, Custom6 (PosPrinterColorCapabilities_Custom6) = 64, Cyan (PosPrinterColorCapabilities_Cyan) = 128, Magenta (PosPrinterColorCapabilities_Magenta) = 256, Yellow (PosPrinterColorCapabilities_Yellow) = 512, Full (PosPrinterColorCapabilities_Full) = 1024,
		}}
		RT_ENUM! { enum PosPrinterColorCartridge: i32 {
			Unknown (PosPrinterColorCartridge_Unknown) = 0, Primary (PosPrinterColorCartridge_Primary) = 1, Custom1 (PosPrinterColorCartridge_Custom1) = 2, Custom2 (PosPrinterColorCartridge_Custom2) = 3, Custom3 (PosPrinterColorCartridge_Custom3) = 4, Custom4 (PosPrinterColorCartridge_Custom4) = 5, Custom5 (PosPrinterColorCartridge_Custom5) = 6, Custom6 (PosPrinterColorCartridge_Custom6) = 7, Cyan (PosPrinterColorCartridge_Cyan) = 8, Magenta (PosPrinterColorCartridge_Magenta) = 9, Yellow (PosPrinterColorCartridge_Yellow) = 10,
		}}
		RT_ENUM! { enum PosPrinterMarkFeedCapabilities: u32 {
			None (PosPrinterMarkFeedCapabilities_None) = 0, ToTakeUp (PosPrinterMarkFeedCapabilities_ToTakeUp) = 1, ToCutter (PosPrinterMarkFeedCapabilities_ToCutter) = 2, ToCurrentTopOfForm (PosPrinterMarkFeedCapabilities_ToCurrentTopOfForm) = 4, ToNextTopOfForm (PosPrinterMarkFeedCapabilities_ToNextTopOfForm) = 8,
		}}
		RT_ENUM! { enum PosPrinterRuledLineCapabilities: u32 {
			None (PosPrinterRuledLineCapabilities_None) = 0, Horizontal (PosPrinterRuledLineCapabilities_Horizontal) = 1, Vertical (PosPrinterRuledLineCapabilities_Vertical) = 2,
		}}
		RT_ENUM! { enum PosPrinterPrintSide: i32 {
			Unknown (PosPrinterPrintSide_Unknown) = 0, Side1 (PosPrinterPrintSide_Side1) = 1, Side2 (PosPrinterPrintSide_Side2) = 2,
		}}
		RT_ENUM! { enum PosPrinterLineDirection: i32 {
			Horizontal (PosPrinterLineDirection_Horizontal) = 0, Vertical (PosPrinterLineDirection_Vertical) = 1,
		}}
		RT_ENUM! { enum PosPrinterLineStyle: i32 {
			SingleSolid (PosPrinterLineStyle_SingleSolid) = 0, DoubleSolid (PosPrinterLineStyle_DoubleSolid) = 1, Broken (PosPrinterLineStyle_Broken) = 2, Chain (PosPrinterLineStyle_Chain) = 3,
		}}
		RT_ENUM! { enum PosPrinterMarkFeedKind: i32 {
			ToTakeUp (PosPrinterMarkFeedKind_ToTakeUp) = 0, ToCutter (PosPrinterMarkFeedKind_ToCutter) = 1, ToCurrentTopOfForm (PosPrinterMarkFeedKind_ToCurrentTopOfForm) = 2, ToNextTopOfForm (PosPrinterMarkFeedKind_ToNextTopOfForm) = 3,
		}}
		RT_ENUM! { enum PosPrinterAlignment: i32 {
			Left (PosPrinterAlignment_Left) = 0, Center (PosPrinterAlignment_Center) = 1, Right (PosPrinterAlignment_Right) = 2,
		}}
		RT_ENUM! { enum PosPrinterBarcodeTextPosition: i32 {
			None (PosPrinterBarcodeTextPosition_None) = 0, Above (PosPrinterBarcodeTextPosition_Above) = 1, Below (PosPrinterBarcodeTextPosition_Below) = 2,
		}}
		RT_ENUM! { enum PosPrinterRotation: i32 {
			Normal (PosPrinterRotation_Normal) = 0, Right90 (PosPrinterRotation_Right90) = 1, Left90 (PosPrinterRotation_Left90) = 2, Rotate180 (PosPrinterRotation_Rotate180) = 3,
		}}
		RT_ENUM! { enum PosPrinterStatusKind: i32 {
			Online (PosPrinterStatusKind_Online) = 0, Off (PosPrinterStatusKind_Off) = 1, Offline (PosPrinterStatusKind_Offline) = 2, OffOrOffline (PosPrinterStatusKind_OffOrOffline) = 3, Extended (PosPrinterStatusKind_Extended) = 4,
		}}
		RT_ENUM! { enum CashDrawerStatusKind: i32 {
			Online (CashDrawerStatusKind_Online) = 0, Off (CashDrawerStatusKind_Off) = 1, Offline (CashDrawerStatusKind_Offline) = 2, OffOrOffline (CashDrawerStatusKind_OffOrOffline) = 3, Extended (CashDrawerStatusKind_Extended) = 4,
		}}
		RT_ENUM! { enum BarcodeScannerStatus: i32 {
			Online (BarcodeScannerStatus_Online) = 0, Off (BarcodeScannerStatus_Off) = 1, Offline (BarcodeScannerStatus_Offline) = 2, OffOrOffline (BarcodeScannerStatus_OffOrOffline) = 3, Extended (BarcodeScannerStatus_Extended) = 4,
		}}
		DEFINE_IID!(IID_IUnifiedPosErrorData, 731483194, 21852, 18569, 142, 216, 197, 153, 187, 58, 113, 42);
		RT_INTERFACE!{interface IUnifiedPosErrorData(IUnifiedPosErrorDataVtbl): IInspectable(IInspectableVtbl) [IID_IUnifiedPosErrorData] {
			fn get_Message(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Severity(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::UnifiedPosErrorSeverity) -> ::w::HRESULT,
			fn get_Reason(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::UnifiedPosErrorReason) -> ::w::HRESULT,
			fn get_ExtendedReason(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		RT_CLASS!(UnifiedPosErrorData: ::rt::gen::windows::devices::pointofservice::IUnifiedPosErrorData);
		DEFINE_IID!(IID_IBarcodeScannerStatusUpdatedEventArgs, 895321478, 40003, 17963, 169, 26, 129, 109, 201, 127, 69, 44);
		RT_INTERFACE!{interface IBarcodeScannerStatusUpdatedEventArgs(IBarcodeScannerStatusUpdatedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScannerStatusUpdatedEventArgs] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::BarcodeScannerStatus) -> ::w::HRESULT,
			fn get_ExtendedStatus(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		RT_CLASS!(BarcodeScannerStatusUpdatedEventArgs: ::rt::gen::windows::devices::pointofservice::IBarcodeScannerStatusUpdatedEventArgs);
		DEFINE_IID!(IID_IBarcodeSymbologiesStatics, 3397732795, 1746, 17396, 164, 75, 198, 32, 103, 159, 216, 208);
		RT_INTERFACE!{interface IBarcodeSymbologiesStatics(IBarcodeSymbologiesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeSymbologiesStatics] {
			fn get_Unknown(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean8(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean8Add2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean8Add5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Eanv(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_EanvAdd2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_EanvAdd5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean13(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean13Add2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean13Add5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Isbn(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_IsbnAdd5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ismn(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_IsmnAdd2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_IsmnAdd5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Issn(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_IssnAdd2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_IssnAdd5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean99(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean99Add2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ean99Add5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Upca(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UpcaAdd2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UpcaAdd5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Upce(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UpceAdd2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UpceAdd5(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UpcCoupon(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_TfStd(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_TfDis(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_TfInt(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_TfInd(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_TfMat(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_TfIata(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Gs1DatabarType1(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Gs1DatabarType2(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Gs1DatabarType3(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code39(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code39Ex(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Trioptic39(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code32(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Pzn(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code93(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code93Ex(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code128(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Gs1128(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Gs1128Coupon(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UccEan128(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Sisac(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Isbt(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Codabar(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code11(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Msi(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Plessey(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Telepen(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code16k(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_CodablockA(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_CodablockF(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Codablock128(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Code49(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Aztec(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_DataCode(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_DataMatrix(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_HanXin(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Maxicode(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_MicroPdf417(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_MicroQr(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Pdf417(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Qr(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_MsTag(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ccab(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ccc(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Tlc39(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_AusPost(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_CanPost(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ChinaPost(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_DutchKix(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_InfoMail(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ItalianPost25(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ItalianPost39(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_JapanPost(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_KoreanPost(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_SwedenPost(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UkPost(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UsIntelligent(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UsIntelligentPkg(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UsPlanet(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UsPostNet(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Us4StateFics(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_OcrA(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_OcrB(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Micr(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ExtendedBase(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn GetName(&mut self, scanDataType: u32, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBarcodeScannerDataReceivedEventArgs, 1110747106, 60823, 18045, 173, 43, 1, 228, 67, 19, 169, 41);
		RT_INTERFACE!{interface IBarcodeScannerDataReceivedEventArgs(IBarcodeScannerDataReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScannerDataReceivedEventArgs] {
			fn get_Report(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::BarcodeScannerReport) -> ::w::HRESULT
		}}
		RT_CLASS!(BarcodeScannerReport: ::rt::gen::windows::devices::pointofservice::IBarcodeScannerReport);
		RT_CLASS!(BarcodeScannerDataReceivedEventArgs: ::rt::gen::windows::devices::pointofservice::IBarcodeScannerDataReceivedEventArgs);
		DEFINE_IID!(IID_IBarcodeScannerReport, 1558501552, 42121, 19350, 134, 196, 240, 191, 138, 55, 117, 61);
		RT_INTERFACE!{interface IBarcodeScannerReport(IBarcodeScannerReportVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScannerReport] {
			fn get_ScanDataType(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ScanData(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_ScanDataLabel(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBarcodeScannerErrorOccurredEventArgs, 751984687, 53050, 16386, 167, 90, 197, 236, 70, 143, 10, 32);
		RT_INTERFACE!{interface IBarcodeScannerErrorOccurredEventArgs(IBarcodeScannerErrorOccurredEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScannerErrorOccurredEventArgs] {
			fn get_PartialInputData(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::BarcodeScannerReport) -> ::w::HRESULT,
			fn get_IsRetriable(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_ErrorData(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::UnifiedPosErrorData) -> ::w::HRESULT
		}}
		RT_CLASS!(BarcodeScannerErrorOccurredEventArgs: ::rt::gen::windows::devices::pointofservice::IBarcodeScannerErrorOccurredEventArgs);
		DEFINE_IID!(IID_IBarcodeScannerImagePreviewReceivedEventArgs, 4088913541, 28299, 17230, 159, 88, 6, 239, 38, 188, 75, 175);
		RT_INTERFACE!{interface IBarcodeScannerImagePreviewReceivedEventArgs(IBarcodeScannerImagePreviewReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScannerImagePreviewReceivedEventArgs] {
			fn get_Preview(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType) -> ::w::HRESULT
		}}
		RT_CLASS!(BarcodeScannerImagePreviewReceivedEventArgs: ::rt::gen::windows::devices::pointofservice::IBarcodeScannerImagePreviewReceivedEventArgs);
		DEFINE_IID!(IID_IBarcodeScannerCapabilities, 3322319332, 62152, 17440, 163, 7, 177, 46, 246, 98, 40, 87);
		RT_INTERFACE!{interface IBarcodeScannerCapabilities(IBarcodeScannerCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScannerCapabilities] {
			fn get_PowerReportingType(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::UnifiedPosPowerReportingType) -> ::w::HRESULT,
			fn get_IsStatisticsReportingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsStatisticsUpdatingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsImagePreviewSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBarcodeScannerCapabilities1, 2388308969, 3628, 18223, 161, 204, 238, 128, 84, 182, 166, 132);
		RT_INTERFACE!{interface IBarcodeScannerCapabilities1(IBarcodeScannerCapabilities1Vtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScannerCapabilities1] {
			fn get_IsSoftwareTriggerSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(BarcodeScannerCapabilities: ::rt::gen::windows::devices::pointofservice::IBarcodeScannerCapabilities);
		DEFINE_IID!(IID_IBarcodeScannerStatics, 1561419631, 55881, 16872, 140, 140, 240, 203, 98, 169, 196, 252);
		RT_INTERFACE!{interface IBarcodeScannerStatics(IBarcodeScannerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScannerStatics] {
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::BarcodeScanner>) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::BarcodeScanner>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(BarcodeScanner: ::rt::gen::windows::devices::pointofservice::IBarcodeScanner);
		DEFINE_IID!(IID_IBarcodeScanner, 3198369286, 45668, 20227, 169, 193, 69, 178, 15, 1, 19, 79);
		RT_INTERFACE!{interface IBarcodeScanner(IBarcodeScannerVtbl): IInspectable(IInspectableVtbl) [IID_IBarcodeScanner] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Capabilities(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::BarcodeScannerCapabilities) -> ::w::HRESULT,
			fn ClaimScannerAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner>) -> ::w::HRESULT,
			fn CheckHealthAsync(&mut self, level: ::rt::gen::windows::devices::pointofservice::UnifiedPosHealthCheckLevel, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&str>) -> ::w::HRESULT,
			fn GetSupportedSymbologiesAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<u32>>) -> ::w::HRESULT,
			fn IsSymbologySupportedAsync(&mut self, barcodeSymbology: u32, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn RetrieveStatisticsAsync(&mut self, statisticsCategories: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT,
			fn GetSupportedProfiles(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&str>) -> ::w::HRESULT,
			fn IsProfileSupported(&mut self, profile: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn add_StatusUpdated(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::BarcodeScanner, &::rt::gen::windows::devices::pointofservice::BarcodeScannerStatusUpdatedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StatusUpdated(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(ClaimedBarcodeScanner: ::rt::gen::windows::devices::pointofservice::IClaimedBarcodeScanner);
		RT_ENUM! { enum MagneticStripeReaderStatus: i32 {
			Unauthenticated (MagneticStripeReaderStatus_Unauthenticated) = 0, Authenticated (MagneticStripeReaderStatus_Authenticated) = 1, Extended (MagneticStripeReaderStatus_Extended) = 2,
		}}
		RT_ENUM! { enum MagneticStripeReaderAuthenticationProtocol: i32 {
			None (MagneticStripeReaderAuthenticationProtocol_None) = 0, ChallengeResponse (MagneticStripeReaderAuthenticationProtocol_ChallengeResponse) = 1,
		}}
		RT_ENUM! { enum MagneticStripeReaderTrackIds: i32 {
			None (MagneticStripeReaderTrackIds_None) = 0, Track1 (MagneticStripeReaderTrackIds_Track1) = 1, Track2 (MagneticStripeReaderTrackIds_Track2) = 2, Track3 (MagneticStripeReaderTrackIds_Track3) = 4, Track4 (MagneticStripeReaderTrackIds_Track4) = 8,
		}}
		RT_ENUM! { enum MagneticStripeReaderErrorReportingType: i32 {
			CardLevel (MagneticStripeReaderErrorReportingType_CardLevel) = 0, TrackLevel (MagneticStripeReaderErrorReportingType_TrackLevel) = 1,
		}}
		RT_ENUM! { enum MagneticStripeReaderTrackErrorType: i32 {
			None (MagneticStripeReaderTrackErrorType_None) = 0, StartSentinelError (MagneticStripeReaderTrackErrorType_StartSentinelError) = 1, EndSentinelError (MagneticStripeReaderTrackErrorType_EndSentinelError) = 2, ParityError (MagneticStripeReaderTrackErrorType_ParityError) = 3, LrcError (MagneticStripeReaderTrackErrorType_LrcError) = 4, Unknown (MagneticStripeReaderTrackErrorType_Unknown) = -1,
		}}
		DEFINE_IID!(IID_IMagneticStripeReaderEncryptionAlgorithmsStatics, 1404400464, 50139, 18260, 156, 0, 65, 57, 35, 116, 161, 9);
		RT_INTERFACE!{interface IMagneticStripeReaderEncryptionAlgorithmsStatics(IMagneticStripeReaderEncryptionAlgorithmsStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderEncryptionAlgorithmsStatics] {
			fn get_None(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_TripleDesDukpt(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ExtendedBase(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagneticStripeReaderCardTypesStatics, 1385114717, 10630, 18255, 132, 84, 124, 205, 5, 146, 141, 95);
		RT_INTERFACE!{interface IMagneticStripeReaderCardTypesStatics(IMagneticStripeReaderCardTypesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderCardTypesStatics] {
			fn get_Unknown(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Bank(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Aamva(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ExtendedBase(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagneticStripeReaderTrackData, 273479281, 19101, 17518, 171, 197, 32, 64, 35, 7, 186, 54);
		RT_INTERFACE!{interface IMagneticStripeReaderTrackData(IMagneticStripeReaderTrackDataVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderTrackData] {
			fn get_Data(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_DiscretionaryData(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_EncryptedData(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagneticStripeReaderReport, 1784373319, 39344, 16776, 190, 241, 237, 223, 121, 247, 143, 230);
		RT_INTERFACE!{interface IMagneticStripeReaderReport(IMagneticStripeReaderReportVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderReport] {
			fn get_CardType(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Track1(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackData) -> ::w::HRESULT,
			fn get_Track2(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackData) -> ::w::HRESULT,
			fn get_Track3(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackData) -> ::w::HRESULT,
			fn get_Track4(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackData) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &str>) -> ::w::HRESULT,
			fn get_CardAuthenticationData(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_CardAuthenticationDataLength(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_AdditionalSecurityInformation(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		RT_CLASS!(MagneticStripeReaderTrackData: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReaderTrackData);
		DEFINE_IID!(IID_IMagneticStripeReaderBankCardDataReceivedEventArgs, 781551651, 41754, 18275, 136, 44, 35, 114, 94, 57, 176, 142);
		RT_INTERFACE!{interface IMagneticStripeReaderBankCardDataReceivedEventArgs(IMagneticStripeReaderBankCardDataReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderBankCardDataReceivedEventArgs] {
			fn get_Report(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderReport) -> ::w::HRESULT,
			fn get_AccountNumber(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ExpirationDate(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ServiceCode(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Title(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_FirstName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_MiddleInitial(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Surname(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Suffix(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(MagneticStripeReaderReport: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReaderReport);
		DEFINE_IID!(IID_IMagneticStripeReaderAamvaCardDataReceivedEventArgs, 172735825, 49942, 18704, 135, 243, 122, 98, 186, 134, 45, 49);
		RT_INTERFACE!{interface IMagneticStripeReaderAamvaCardDataReceivedEventArgs(IMagneticStripeReaderAamvaCardDataReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderAamvaCardDataReceivedEventArgs] {
			fn get_Report(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderReport) -> ::w::HRESULT,
			fn get_LicenseNumber(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ExpirationDate(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Restrictions(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Class(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Endorsements(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_BirthDate(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_FirstName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Surname(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Suffix(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Gender(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_HairColor(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_EyeColor(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Height(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Weight(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Address(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_City(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_State(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_PostalCode(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagneticStripeReaderVendorSpecificCardDataReceivedEventArgs, 2936689940, 22988, 19040, 153, 232, 153, 165, 61, 172, 229, 170);
		RT_INTERFACE!{interface IMagneticStripeReaderVendorSpecificCardDataReceivedEventArgs(IMagneticStripeReaderVendorSpecificCardDataReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderVendorSpecificCardDataReceivedEventArgs] {
			fn get_Report(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderReport) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagneticStripeReaderErrorOccurredEventArgs, 535689565, 11396, 16813, 183, 120, 242, 53, 106, 120, 154, 177);
		RT_INTERFACE!{interface IMagneticStripeReaderErrorOccurredEventArgs(IMagneticStripeReaderErrorOccurredEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderErrorOccurredEventArgs] {
			fn get_Track1Status(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackErrorType) -> ::w::HRESULT,
			fn get_Track2Status(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackErrorType) -> ::w::HRESULT,
			fn get_Track3Status(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackErrorType) -> ::w::HRESULT,
			fn get_Track4Status(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackErrorType) -> ::w::HRESULT,
			fn get_ErrorData(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::UnifiedPosErrorData) -> ::w::HRESULT,
			fn get_PartialInputData(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderReport) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagneticStripeReaderStatusUpdatedEventArgs, 164391856, 12898, 16413, 158, 138, 232, 13, 99, 88, 144, 107);
		RT_INTERFACE!{interface IMagneticStripeReaderStatusUpdatedEventArgs(IMagneticStripeReaderStatusUpdatedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderStatusUpdatedEventArgs] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderStatus) -> ::w::HRESULT,
			fn get_ExtendedStatus(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		RT_CLASS!(MagneticStripeReaderBankCardDataReceivedEventArgs: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReaderBankCardDataReceivedEventArgs);
		RT_CLASS!(MagneticStripeReaderAamvaCardDataReceivedEventArgs: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReaderAamvaCardDataReceivedEventArgs);
		RT_CLASS!(MagneticStripeReaderVendorSpecificCardDataReceivedEventArgs: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReaderVendorSpecificCardDataReceivedEventArgs);
		RT_CLASS!(MagneticStripeReaderErrorOccurredEventArgs: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReaderErrorOccurredEventArgs);
		RT_CLASS!(MagneticStripeReaderStatusUpdatedEventArgs: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReaderStatusUpdatedEventArgs);
		DEFINE_IID!(IID_IMagneticStripeReaderCapabilities, 1898479772, 50240, 17570, 164, 103, 70, 145, 117, 208, 40, 150);
		RT_INTERFACE!{interface IMagneticStripeReaderCapabilities(IMagneticStripeReaderCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderCapabilities] {
			fn get_CardAuthentication(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SupportedEncryptionAlgorithms(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_AuthenticationLevel(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderAuthenticationLevel) -> ::w::HRESULT,
			fn get_IsIsoSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsJisOneSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsJisTwoSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_PowerReportingType(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::UnifiedPosPowerReportingType) -> ::w::HRESULT,
			fn get_IsStatisticsReportingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsStatisticsUpdatingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsTrackDataMaskingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsTransmitSentinelsSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(MagneticStripeReaderCapabilities: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReaderCapabilities);
		RT_CLASS!(ClaimedMagneticStripeReader: ::rt::gen::windows::devices::pointofservice::IClaimedMagneticStripeReader);
		DEFINE_IID!(IID_IMagneticStripeReaderStatics, 3294604106, 61399, 18272, 165, 206, 21, 176, 228, 126, 148, 235);
		RT_INTERFACE!{interface IMagneticStripeReaderStatics(IMagneticStripeReaderStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReaderStatics] {
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::MagneticStripeReader>) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::MagneticStripeReader>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(MagneticStripeReader: ::rt::gen::windows::devices::pointofservice::IMagneticStripeReader);
		DEFINE_IID!(IID_IMagneticStripeReader, 445820949, 18371, 18058, 147, 51, 12, 101, 23, 87, 72, 131);
		RT_INTERFACE!{interface IMagneticStripeReader(IMagneticStripeReaderVtbl): IInspectable(IInspectableVtbl) [IID_IMagneticStripeReader] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Capabilities(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderCapabilities) -> ::w::HRESULT,
			fn get_SupportedCardTypes(&mut self, out: *mut *mut u32) -> ::w::HRESULT,
			fn get_DeviceAuthenticationProtocol(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderAuthenticationProtocol) -> ::w::HRESULT,
			fn CheckHealthAsync(&mut self, level: ::rt::gen::windows::devices::pointofservice::UnifiedPosHealthCheckLevel, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&str>) -> ::w::HRESULT,
			fn ClaimReaderAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader>) -> ::w::HRESULT,
			fn RetrieveStatisticsAsync(&mut self, statisticsCategories: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT,
			fn GetErrorReportingType(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderErrorReportingType) -> ::w::HRESULT,
			fn add_StatusUpdated(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::MagneticStripeReader, &::rt::gen::windows::devices::pointofservice::MagneticStripeReaderStatusUpdatedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StatusUpdated(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPosPrinterCharacterSetIdsStatics, 1550884607, 28826, 20455, 178, 21, 6, 167, 72, 163, 139, 57);
		RT_INTERFACE!{interface IPosPrinterCharacterSetIdsStatics(IPosPrinterCharacterSetIdsStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPosPrinterCharacterSetIdsStatics] {
			fn get_Utf16LE(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ascii(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Ansi(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICommonPosPrintStationCapabilities, 3730526922, 57390, 16617, 158, 94, 27, 72, 142, 106, 172, 252);
		RT_INTERFACE!{interface ICommonPosPrintStationCapabilities(ICommonPosPrintStationCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_ICommonPosPrintStationCapabilities] {
			fn get_IsPrinterPresent(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDualColorSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_ColorCartridgeCapabilities(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::PosPrinterColorCapabilities) -> ::w::HRESULT,
			fn get_CartridgeSensors(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::PosPrinterCartridgeSensors) -> ::w::HRESULT,
			fn get_IsBoldSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsItalicSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsUnderlineSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDoubleHighPrintSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDoubleWidePrintSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDoubleHighDoubleWidePrintSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsPaperEmptySensorSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsPaperNearEndSensorSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_SupportedCharactersPerLine(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<u32>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICommonReceiptSlipCapabilities, 153643915, 39027, 19717, 191, 190, 71, 39, 166, 3, 143, 105);
		RT_INTERFACE!{interface ICommonReceiptSlipCapabilities(ICommonReceiptSlipCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_ICommonReceiptSlipCapabilities] {
			fn get_IsBarcodeSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsBitmapSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsLeft90RotationSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsRight90RotationSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Is180RotationSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsPrintAreaSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_RuledLineCapabilities(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::PosPrinterRuledLineCapabilities) -> ::w::HRESULT,
			fn get_SupportedBarcodeRotations(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<::rt::gen::windows::devices::pointofservice::PosPrinterRotation>) -> ::w::HRESULT,
			fn get_SupportedBitmapRotations(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<::rt::gen::windows::devices::pointofservice::PosPrinterRotation>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IReceiptPrinterCapabilities, 3102782863, 20904, 17404, 155, 213, 141, 226, 114, 166, 65, 91);
		RT_INTERFACE!{interface IReceiptPrinterCapabilities(IReceiptPrinterCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_IReceiptPrinterCapabilities] {
			fn get_CanCutPaper(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsStampSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_MarkFeedCapabilities(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::PosPrinterMarkFeedCapabilities) -> ::w::HRESULT
		}}
		RT_CLASS!(ReceiptPrinterCapabilities: ::rt::gen::windows::devices::pointofservice::IReceiptPrinterCapabilities);
		DEFINE_IID!(IID_ISlipPrinterCapabilities, 2578539417, 18572, 16727, 138, 194, 159, 87, 247, 8, 211, 219);
		RT_INTERFACE!{interface ISlipPrinterCapabilities(ISlipPrinterCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_ISlipPrinterCapabilities] {
			fn get_IsFullLengthSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsBothSidesPrintingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(SlipPrinterCapabilities: ::rt::gen::windows::devices::pointofservice::ISlipPrinterCapabilities);
		DEFINE_IID!(IID_IJournalPrinterCapabilities, 995937347, 57415, 17507, 187, 88, 23, 181, 186, 29, 128, 86);
		RT_INTERFACE!{interface IJournalPrinterCapabilities(IJournalPrinterCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_IJournalPrinterCapabilities] {
			
		}}
		RT_CLASS!(JournalPrinterCapabilities: ::rt::gen::windows::devices::pointofservice::IJournalPrinterCapabilities);
		DEFINE_IID!(IID_IPosPrinterCapabilities, 3454621473, 17280, 18821, 173, 197, 57, 219, 48, 205, 147, 188);
		RT_INTERFACE!{interface IPosPrinterCapabilities(IPosPrinterCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_IPosPrinterCapabilities] {
			fn get_PowerReportingType(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::UnifiedPosPowerReportingType) -> ::w::HRESULT,
			fn get_IsStatisticsReportingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsStatisticsUpdatingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_DefaultCharacterSet(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_HasCoverSensor(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_CanMapCharacterSet(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsTransactionSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Receipt(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::ReceiptPrinterCapabilities) -> ::w::HRESULT,
			fn get_Slip(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::SlipPrinterCapabilities) -> ::w::HRESULT,
			fn get_Journal(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::JournalPrinterCapabilities) -> ::w::HRESULT
		}}
		RT_CLASS!(PosPrinterCapabilities: ::rt::gen::windows::devices::pointofservice::IPosPrinterCapabilities);
		DEFINE_IID!(IID_IPosPrinterStatus, 3522217776, 55872, 17192, 191, 118, 81, 86, 250, 51, 183, 71);
		RT_INTERFACE!{interface IPosPrinterStatus(IPosPrinterStatusVtbl): IInspectable(IInspectableVtbl) [IID_IPosPrinterStatus] {
			fn get_StatusKind(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::PosPrinterStatusKind) -> ::w::HRESULT,
			fn get_ExtendedStatus(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		RT_CLASS!(PosPrinterStatus: ::rt::gen::windows::devices::pointofservice::IPosPrinterStatus);
		DEFINE_IID!(IID_IPosPrinterStatusUpdatedEventArgs, 786139103, 5030, 17037, 186, 129, 176, 231, 195, 229, 163, 205);
		RT_INTERFACE!{interface IPosPrinterStatusUpdatedEventArgs(IPosPrinterStatusUpdatedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPosPrinterStatusUpdatedEventArgs] {
			fn get_Status(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::PosPrinterStatus) -> ::w::HRESULT
		}}
		RT_CLASS!(PosPrinterStatusUpdatedEventArgs: ::rt::gen::windows::devices::pointofservice::IPosPrinterStatusUpdatedEventArgs);
		DEFINE_IID!(IID_IPosPrinterReleaseDeviceRequestedEventArgs, 734765913, 7407, 16562, 158, 203, 249, 39, 248, 86, 174, 60);
		RT_INTERFACE!{interface IPosPrinterReleaseDeviceRequestedEventArgs(IPosPrinterReleaseDeviceRequestedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPosPrinterReleaseDeviceRequestedEventArgs] {
			
		}}
		RT_CLASS!(PosPrinterReleaseDeviceRequestedEventArgs: ::rt::gen::windows::devices::pointofservice::IPosPrinterReleaseDeviceRequestedEventArgs);
		DEFINE_IID!(IID_IPosPrinterStatics, 2363544810, 4911, 19679, 166, 74, 45, 13, 124, 150, 168, 91);
		RT_INTERFACE!{interface IPosPrinterStatics(IPosPrinterStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPosPrinterStatics] {
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::PosPrinter>) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::PosPrinter>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(PosPrinter: ::rt::gen::windows::devices::pointofservice::IPosPrinter);
		DEFINE_IID!(IID_IPosPrinter, 704889102, 39449, 18945, 153, 79, 18, 223, 173, 106, 220, 191);
		RT_INTERFACE!{interface IPosPrinter(IPosPrinterVtbl): IInspectable(IInspectableVtbl) [IID_IPosPrinter] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Capabilities(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::PosPrinterCapabilities) -> ::w::HRESULT,
			fn get_SupportedCharacterSets(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<u32>) -> ::w::HRESULT,
			fn get_SupportedTypeFaces(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&str>) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::PosPrinterStatus) -> ::w::HRESULT,
			fn ClaimPrinterAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::ClaimedPosPrinter>) -> ::w::HRESULT,
			fn CheckHealthAsync(&mut self, level: ::rt::gen::windows::devices::pointofservice::UnifiedPosHealthCheckLevel, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&str>) -> ::w::HRESULT,
			fn GetStatisticsAsync(&mut self, statisticsCategories: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&str>) -> ::w::HRESULT,
			fn add_StatusUpdated(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::PosPrinter, &::rt::gen::windows::devices::pointofservice::PosPrinterStatusUpdatedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StatusUpdated(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(ClaimedPosPrinter: ::rt::gen::windows::devices::pointofservice::IClaimedPosPrinter);
		DEFINE_IID!(IID_IPosPrinterJob, 2593390684, 1557, 17809, 165, 143, 48, 248, 126, 223, 226, 228);
		RT_INTERFACE!{interface IPosPrinterJob(IPosPrinterJobVtbl): IInspectable(IInspectableVtbl) [IID_IPosPrinterJob] {
			fn Print(&mut self, data: ::w::HSTRING) -> ::w::HRESULT,
			fn PrintLine(&mut self, data: ::w::HSTRING) -> ::w::HRESULT,
			fn PrintNewline(&mut self) -> ::w::HRESULT,
			fn ExecuteAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IReceiptOrSlipJob, 1394710974, 51395, 19906, 137, 233, 92, 74, 55, 179, 77, 220);
		RT_INTERFACE!{interface IReceiptOrSlipJob(IReceiptOrSlipJobVtbl): IInspectable(IInspectableVtbl) [IID_IReceiptOrSlipJob] {
			fn SetBarcodeRotation(&mut self, value: ::rt::gen::windows::devices::pointofservice::PosPrinterRotation) -> ::w::HRESULT,
			fn SetPrintRotation(&mut self, value: ::rt::gen::windows::devices::pointofservice::PosPrinterRotation, includeBitmaps: ::w::BOOL) -> ::w::HRESULT,
			fn SetPrintArea(&mut self, value: ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn SetBitmap(&mut self, bitmapNumber: u32, bitmap: *mut ::rt::gen::windows::graphics::imaging::BitmapFrame, alignment: ::rt::gen::windows::devices::pointofservice::PosPrinterAlignment) -> ::w::HRESULT,
			fn SetBitmapCustomWidthStandardAlign(&mut self, bitmapNumber: u32, bitmap: *mut ::rt::gen::windows::graphics::imaging::BitmapFrame, alignment: ::rt::gen::windows::devices::pointofservice::PosPrinterAlignment, width: u32) -> ::w::HRESULT,
			fn SetCustomAlignedBitmap(&mut self, bitmapNumber: u32, bitmap: *mut ::rt::gen::windows::graphics::imaging::BitmapFrame, alignmentDistance: u32) -> ::w::HRESULT,
			fn SetBitmapCustomWidthCustomAlign(&mut self, bitmapNumber: u32, bitmap: *mut ::rt::gen::windows::graphics::imaging::BitmapFrame, alignmentDistance: u32, width: u32) -> ::w::HRESULT,
			fn PrintSavedBitmap(&mut self, bitmapNumber: u32) -> ::w::HRESULT,
			fn DrawRuledLine(&mut self, positionList: ::w::HSTRING, lineDirection: ::rt::gen::windows::devices::pointofservice::PosPrinterLineDirection, lineWidth: u32, lineStyle: ::rt::gen::windows::devices::pointofservice::PosPrinterLineStyle, lineColor: u32) -> ::w::HRESULT,
			fn PrintBarcode(&mut self, data: ::w::HSTRING, symbology: u32, height: u32, width: u32, textPosition: ::rt::gen::windows::devices::pointofservice::PosPrinterBarcodeTextPosition, alignment: ::rt::gen::windows::devices::pointofservice::PosPrinterAlignment) -> ::w::HRESULT,
			fn PrintBarcodeCustomAlign(&mut self, data: ::w::HSTRING, symbology: u32, height: u32, width: u32, textPosition: ::rt::gen::windows::devices::pointofservice::PosPrinterBarcodeTextPosition, alignmentDistance: u32) -> ::w::HRESULT,
			fn PrintBitmap(&mut self, bitmap: *mut ::rt::gen::windows::graphics::imaging::BitmapFrame, alignment: ::rt::gen::windows::devices::pointofservice::PosPrinterAlignment) -> ::w::HRESULT,
			fn PrintBitmapCustomWidthStandardAlign(&mut self, bitmap: *mut ::rt::gen::windows::graphics::imaging::BitmapFrame, alignment: ::rt::gen::windows::devices::pointofservice::PosPrinterAlignment, width: u32) -> ::w::HRESULT,
			fn PrintCustomAlignedBitmap(&mut self, bitmap: *mut ::rt::gen::windows::graphics::imaging::BitmapFrame, alignmentDistance: u32) -> ::w::HRESULT,
			fn PrintBitmapCustomWidthCustomAlign(&mut self, bitmap: *mut ::rt::gen::windows::graphics::imaging::BitmapFrame, alignmentDistance: u32, width: u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IReceiptPrintJob, 2861958766, 44205, 19321, 157, 15, 192, 207, 192, 141, 199, 123);
		RT_INTERFACE!{interface IReceiptPrintJob(IReceiptPrintJobVtbl): IInspectable(IInspectableVtbl) [IID_IReceiptPrintJob] {
			fn MarkFeed(&mut self, kind: ::rt::gen::windows::devices::pointofservice::PosPrinterMarkFeedKind) -> ::w::HRESULT,
			fn CutPaper(&mut self, percentage: f64) -> ::w::HRESULT,
			fn CutPaperDefault(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(ReceiptPrintJob: ::rt::gen::windows::devices::pointofservice::IReceiptPrintJob);
		RT_CLASS!(SlipPrintJob: ::rt::gen::windows::devices::pointofservice::IReceiptOrSlipJob);
		RT_CLASS!(JournalPrintJob: ::rt::gen::windows::devices::pointofservice::IPosPrinterJob);
		DEFINE_IID!(IID_ICommonClaimedPosPrinterStation, 3085657768, 65162, 19707, 139, 66, 227, 91, 40, 12, 178, 124);
		RT_INTERFACE!{interface ICommonClaimedPosPrinterStation(ICommonClaimedPosPrinterStationVtbl): IInspectable(IInspectableVtbl) [IID_ICommonClaimedPosPrinterStation] {
			fn put_CharactersPerLine(&mut self, value: u32) -> ::w::HRESULT,
			fn get_CharactersPerLine(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_LineHeight(&mut self, value: u32) -> ::w::HRESULT,
			fn get_LineHeight(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_LineSpacing(&mut self, value: u32) -> ::w::HRESULT,
			fn get_LineSpacing(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_LineWidth(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_IsLetterQuality(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_IsLetterQuality(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsPaperNearEnd(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_ColorCartridge(&mut self, value: ::rt::gen::windows::devices::pointofservice::PosPrinterColorCartridge) -> ::w::HRESULT,
			fn get_ColorCartridge(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::PosPrinterColorCartridge) -> ::w::HRESULT,
			fn get_IsCoverOpen(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsCartridgeRemoved(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsCartridgeEmpty(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsHeadCleaning(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsPaperEmpty(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsReadyToPrint(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn ValidateData(&mut self, data: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IClaimedReceiptPrinter, 2597485172, 56673, 20194, 152, 55, 91, 93, 114, 213, 56, 185);
		RT_INTERFACE!{interface IClaimedReceiptPrinter(IClaimedReceiptPrinterVtbl): IInspectable(IInspectableVtbl) [IID_IClaimedReceiptPrinter] {
			fn get_SidewaysMaxLines(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_SidewaysMaxChars(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_LinesToPaperCut(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_PageSize(&mut self, out: *mut ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn get_PrintArea(&mut self, out: *mut ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn CreateJob(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::ReceiptPrintJob) -> ::w::HRESULT
		}}
		RT_CLASS!(ClaimedReceiptPrinter: ::rt::gen::windows::devices::pointofservice::IClaimedReceiptPrinter);
		DEFINE_IID!(IID_IClaimedSlipPrinter, 3177050098, 44944, 20106, 183, 123, 227, 174, 156, 166, 58, 127);
		RT_INTERFACE!{interface IClaimedSlipPrinter(IClaimedSlipPrinterVtbl): IInspectable(IInspectableVtbl) [IID_IClaimedSlipPrinter] {
			fn get_SidewaysMaxLines(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_SidewaysMaxChars(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_MaxLines(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_LinesNearEndToEnd(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_PrintSide(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::PosPrinterPrintSide) -> ::w::HRESULT,
			fn get_PageSize(&mut self, out: *mut ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn get_PrintArea(&mut self, out: *mut ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn OpenJaws(&mut self) -> ::w::HRESULT,
			fn CloseJaws(&mut self) -> ::w::HRESULT,
			fn InsertSlipAsync(&mut self, timeout: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn RemoveSlipAsync(&mut self, timeout: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn ChangePrintSide(&mut self, printSide: ::rt::gen::windows::devices::pointofservice::PosPrinterPrintSide) -> ::w::HRESULT,
			fn CreateJob(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::SlipPrintJob) -> ::w::HRESULT
		}}
		RT_CLASS!(ClaimedSlipPrinter: ::rt::gen::windows::devices::pointofservice::IClaimedSlipPrinter);
		DEFINE_IID!(IID_IClaimedJournalPrinter, 1743390256, 20861, 18559, 159, 223, 210, 224, 160, 162, 100, 165);
		RT_INTERFACE!{interface IClaimedJournalPrinter(IClaimedJournalPrinterVtbl): IInspectable(IInspectableVtbl) [IID_IClaimedJournalPrinter] {
			fn CreateJob(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::JournalPrintJob) -> ::w::HRESULT
		}}
		RT_CLASS!(ClaimedJournalPrinter: ::rt::gen::windows::devices::pointofservice::IClaimedJournalPrinter);
		DEFINE_IID!(IID_ICashDrawerStatusUpdatedEventArgs, 816507274, 3440, 17820, 149, 83, 135, 225, 36, 197, 36, 136);
		RT_INTERFACE!{interface ICashDrawerStatusUpdatedEventArgs(ICashDrawerStatusUpdatedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ICashDrawerStatusUpdatedEventArgs] {
			fn get_Status(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::CashDrawerStatus) -> ::w::HRESULT
		}}
		RT_CLASS!(CashDrawerStatus: ::rt::gen::windows::devices::pointofservice::ICashDrawerStatus);
		RT_CLASS!(CashDrawerStatusUpdatedEventArgs: ::rt::gen::windows::devices::pointofservice::ICashDrawerStatusUpdatedEventArgs);
		DEFINE_IID!(IID_ICashDrawerStatus, 1807579327, 56481, 19974, 153, 235, 90, 246, 165, 174, 193, 8);
		RT_INTERFACE!{interface ICashDrawerStatus(ICashDrawerStatusVtbl): IInspectable(IInspectableVtbl) [IID_ICashDrawerStatus] {
			fn get_StatusKind(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::CashDrawerStatusKind) -> ::w::HRESULT,
			fn get_ExtendedStatus(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICashDrawerCapabilities, 197582347, 59623, 19231, 177, 209, 62, 80, 26, 208, 130, 71);
		RT_INTERFACE!{interface ICashDrawerCapabilities(ICashDrawerCapabilitiesVtbl): IInspectable(IInspectableVtbl) [IID_ICashDrawerCapabilities] {
			fn get_PowerReportingType(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::UnifiedPosPowerReportingType) -> ::w::HRESULT,
			fn get_IsStatisticsReportingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsStatisticsUpdatingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsStatusReportingSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsStatusMultiDrawerDetectSupported(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDrawerOpenSensorAvailable(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(CashDrawerCapabilities: ::rt::gen::windows::devices::pointofservice::ICashDrawerCapabilities);
		DEFINE_IID!(IID_ICashDrawerEventSourceEventArgs, 1774926785, 5247, 16924, 156, 35, 9, 1, 35, 187, 120, 108);
		RT_INTERFACE!{interface ICashDrawerEventSourceEventArgs(ICashDrawerEventSourceEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ICashDrawerEventSourceEventArgs] {
			fn get_CashDrawer(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::CashDrawer) -> ::w::HRESULT
		}}
		RT_CLASS!(CashDrawer: ::rt::gen::windows::devices::pointofservice::ICashDrawer);
		RT_CLASS!(CashDrawerClosedEventArgs: ::rt::gen::windows::devices::pointofservice::ICashDrawerEventSourceEventArgs);
		RT_CLASS!(CashDrawerOpenedEventArgs: ::rt::gen::windows::devices::pointofservice::ICashDrawerEventSourceEventArgs);
		DEFINE_IID!(IID_ICashDrawerEventSource, 3758548076, 62201, 17455, 141, 214, 6, 193, 10, 66, 39, 186);
		RT_INTERFACE!{interface ICashDrawerEventSource(ICashDrawerEventSourceVtbl): IInspectable(IInspectableVtbl) [IID_ICashDrawerEventSource] {
			fn add_DrawerClosed(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::CashDrawerEventSource, &::rt::gen::windows::devices::pointofservice::CashDrawerClosedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_DrawerClosed(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_DrawerOpened(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::CashDrawerEventSource, &::rt::gen::windows::devices::pointofservice::CashDrawerOpenedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_DrawerOpened(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(CashDrawerEventSource: ::rt::gen::windows::devices::pointofservice::ICashDrawerEventSource);
		DEFINE_IID!(IID_ICashDrawerStatics, 3751843162, 54327, 20479, 181, 71, 221, 169, 105, 164, 248, 131);
		RT_INTERFACE!{interface ICashDrawerStatics(ICashDrawerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ICashDrawerStatics] {
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::CashDrawer>) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::CashDrawer>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICashDrawer, 2676553160, 56916, 19182, 168, 144, 146, 11, 203, 254, 48, 252);
		RT_INTERFACE!{interface ICashDrawer(ICashDrawerVtbl): IInspectable(IInspectableVtbl) [IID_ICashDrawer] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Capabilities(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::CashDrawerCapabilities) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::CashDrawerStatus) -> ::w::HRESULT,
			fn get_IsDrawerOpen(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_DrawerEventSource(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::CashDrawerEventSource) -> ::w::HRESULT,
			fn ClaimDrawerAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::pointofservice::ClaimedCashDrawer>) -> ::w::HRESULT,
			fn CheckHealthAsync(&mut self, level: ::rt::gen::windows::devices::pointofservice::UnifiedPosHealthCheckLevel, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&str>) -> ::w::HRESULT,
			fn GetStatisticsAsync(&mut self, statisticsCategories: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&str>) -> ::w::HRESULT,
			fn add_StatusUpdated(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::CashDrawer, &::rt::gen::windows::devices::pointofservice::CashDrawerStatusUpdatedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StatusUpdated(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(ClaimedCashDrawer: ::rt::gen::windows::devices::pointofservice::IClaimedCashDrawer);
		DEFINE_IID!(IID_ICashDrawerCloseAlarm, 1811451079, 28515, 17166, 171, 59, 149, 215, 95, 251, 232, 127);
		RT_INTERFACE!{interface ICashDrawerCloseAlarm(ICashDrawerCloseAlarmVtbl): IInspectable(IInspectableVtbl) [IID_ICashDrawerCloseAlarm] {
			fn put_AlarmTimeout(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_AlarmTimeout(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn put_BeepFrequency(&mut self, value: u32) -> ::w::HRESULT,
			fn get_BeepFrequency(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_BeepDuration(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_BeepDuration(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn put_BeepDelay(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_BeepDelay(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn add_AlarmTimeoutExpired(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::CashDrawerCloseAlarm, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AlarmTimeoutExpired(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn StartAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT
		}}
		RT_CLASS!(CashDrawerCloseAlarm: ::rt::gen::windows::devices::pointofservice::ICashDrawerCloseAlarm);
		DEFINE_IID!(IID_IClaimedBarcodeScanner, 1248048284, 36772, 17202, 187, 38, 148, 93, 17, 216, 30, 15);
		RT_INTERFACE!{interface IClaimedBarcodeScanner(IClaimedBarcodeScannerVtbl): IInspectable(IInspectableVtbl) [IID_IClaimedBarcodeScanner] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsDisabledOnDataReceived(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDisabledOnDataReceived(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsDecodeDataEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDecodeDataEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn EnableAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn DisableAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn RetainDevice(&mut self) -> ::w::HRESULT,
			fn SetActiveSymbologiesAsync(&mut self, symbologies: *mut ::rt::gen::windows::foundation::collections::IIterable<u32>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn ResetStatisticsAsync(&mut self, statisticsCategories: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn UpdateStatisticsAsync(&mut self, statistics: *mut ::rt::gen::windows::foundation::collections::IIterable<&::rt::gen::windows::foundation::collections::IKeyValuePair<&str, &str>>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn SetActiveProfileAsync(&mut self, profile: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn add_DataReceived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner, &::rt::gen::windows::devices::pointofservice::BarcodeScannerDataReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_DataReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_TriggerPressed(&mut self, handler: *mut ::rt::gen::windows::foundation::EventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_TriggerPressed(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_TriggerReleased(&mut self, handler: *mut ::rt::gen::windows::foundation::EventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_TriggerReleased(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ReleaseDeviceRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::EventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReleaseDeviceRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ImagePreviewReceived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner, &::rt::gen::windows::devices::pointofservice::BarcodeScannerImagePreviewReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ImagePreviewReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ErrorOccurred(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner, &::rt::gen::windows::devices::pointofservice::BarcodeScannerErrorOccurredEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ErrorOccurred(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IClaimedBarcodeScanner1, 4128943372, 34129, 17076, 153, 140, 151, 12, 32, 33, 10, 34);
		RT_INTERFACE!{interface IClaimedBarcodeScanner1(IClaimedBarcodeScanner1Vtbl): IInspectable(IInspectableVtbl) [IID_IClaimedBarcodeScanner1] {
			fn StartSoftwareTriggerAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn StopSoftwareTriggerAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IClaimedMagneticStripeReader, 1197254899, 37911, 18620, 185, 215, 65, 99, 167, 132, 76, 2);
		RT_INTERFACE!{interface IClaimedMagneticStripeReader(IClaimedMagneticStripeReaderVtbl): IInspectable(IInspectableVtbl) [IID_IClaimedMagneticStripeReader] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsDisabledOnDataReceived(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDisabledOnDataReceived(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsDecodeDataEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDecodeDataEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDeviceAuthenticated(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_DataEncryptionAlgorithm(&mut self, value: u32) -> ::w::HRESULT,
			fn get_DataEncryptionAlgorithm(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_TracksToRead(&mut self, value: ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackIds) -> ::w::HRESULT,
			fn get_TracksToRead(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderTrackIds) -> ::w::HRESULT,
			fn put_IsTransmitSentinelsEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_IsTransmitSentinelsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn EnableAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn DisableAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn RetainDevice(&mut self) -> ::w::HRESULT,
			fn SetErrorReportingType(&mut self, value: ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderErrorReportingType) -> ::w::HRESULT,
			fn RetrieveDeviceAuthenticationDataAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT,
			fn AuthenticateDeviceAsync(&mut self, responseToken: *mut u8, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn DeAuthenticateDeviceAsync(&mut self, responseToken: *mut u8, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn UpdateKeyAsync(&mut self, key: ::w::HSTRING, keyName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn ResetStatisticsAsync(&mut self, statisticsCategories: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn UpdateStatisticsAsync(&mut self, statistics: *mut ::rt::gen::windows::foundation::collections::IIterable<&::rt::gen::windows::foundation::collections::IKeyValuePair<&str, &str>>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn add_BankCardDataReceived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader, &::rt::gen::windows::devices::pointofservice::MagneticStripeReaderBankCardDataReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_BankCardDataReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_AamvaCardDataReceived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader, &::rt::gen::windows::devices::pointofservice::MagneticStripeReaderAamvaCardDataReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AamvaCardDataReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_VendorSpecificDataReceived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader, &::rt::gen::windows::devices::pointofservice::MagneticStripeReaderVendorSpecificCardDataReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_VendorSpecificDataReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ReleaseDeviceRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::EventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReleaseDeviceRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_ErrorOccurred(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader, &::rt::gen::windows::devices::pointofservice::MagneticStripeReaderErrorOccurredEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ErrorOccurred(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IClaimedPosPrinter, 1835322892, 57406, 19220, 163, 142, 194, 140, 52, 184, 99, 83);
		RT_INTERFACE!{interface IClaimedPosPrinter(IClaimedPosPrinterVtbl): IInspectable(IInspectableVtbl) [IID_IClaimedPosPrinter] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_CharacterSet(&mut self, value: u32) -> ::w::HRESULT,
			fn get_CharacterSet(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_IsCoverOpen(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsCharacterSetMappingEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_IsCharacterSetMappingEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_MapMode(&mut self, value: ::rt::gen::windows::devices::pointofservice::PosPrinterMapMode) -> ::w::HRESULT,
			fn get_MapMode(&mut self, out: *mut ::rt::gen::windows::devices::pointofservice::PosPrinterMapMode) -> ::w::HRESULT,
			fn get_Receipt(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::ClaimedReceiptPrinter) -> ::w::HRESULT,
			fn get_Slip(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::ClaimedSlipPrinter) -> ::w::HRESULT,
			fn get_Journal(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::ClaimedJournalPrinter) -> ::w::HRESULT,
			fn EnableAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn DisableAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn RetainDeviceAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn ResetStatisticsAsync(&mut self, statisticsCategories: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn UpdateStatisticsAsync(&mut self, statistics: *mut ::rt::gen::windows::foundation::collections::IIterable<&::rt::gen::windows::foundation::collections::IKeyValuePair<&str, &str>>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn add_ReleaseDeviceRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedPosPrinter, &::rt::gen::windows::devices::pointofservice::PosPrinterReleaseDeviceRequestedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReleaseDeviceRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IClaimedCashDrawer, 3393165743, 43960, 17089, 138, 132, 92, 102, 81, 47, 90, 117);
		RT_INTERFACE!{interface IClaimedCashDrawer(IClaimedCashDrawerVtbl): IInspectable(IInspectableVtbl) [IID_IClaimedCashDrawer] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsDrawerOpen(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_CloseAlarm(&mut self, out: *mut *mut ::rt::gen::windows::devices::pointofservice::CashDrawerCloseAlarm) -> ::w::HRESULT,
			fn OpenDrawerAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn EnableAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn DisableAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn RetainDeviceAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn ResetStatisticsAsync(&mut self, statisticsCategories: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn UpdateStatisticsAsync(&mut self, statistics: *mut ::rt::gen::windows::foundation::collections::IIterable<&::rt::gen::windows::foundation::collections::IKeyValuePair<&str, &str>>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn add_ReleaseDeviceRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::pointofservice::ClaimedCashDrawer, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReleaseDeviceRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
} // Windows.Devices.PointOfService
pub mod radios { // Windows.Devices.Radios
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum RadioState: i32 {
			Unknown (RadioState_Unknown) = 0, On (RadioState_On) = 1, Off (RadioState_Off) = 2, Disabled (RadioState_Disabled) = 3,
		}}
		RT_ENUM! { enum RadioKind: i32 {
			Other (RadioKind_Other) = 0, WiFi (RadioKind_WiFi) = 1, MobileBroadband (RadioKind_MobileBroadband) = 2, Bluetooth (RadioKind_Bluetooth) = 3, FM (RadioKind_FM) = 4,
		}}
		RT_ENUM! { enum RadioAccessStatus: i32 {
			Unspecified (RadioAccessStatus_Unspecified) = 0, Allowed (RadioAccessStatus_Allowed) = 1, DeniedByUser (RadioAccessStatus_DeniedByUser) = 2, DeniedBySystem (RadioAccessStatus_DeniedBySystem) = 3,
		}}
		DEFINE_IID!(IID_IRadioStatics, 1605804334, 26571, 18094, 170, 233, 101, 145, 159, 134, 239, 244);
		RT_INTERFACE!{interface IRadioStatics(IRadioStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IRadioStatics] {
			fn GetRadiosAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::radios::Radio>>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::radios::Radio>) -> ::w::HRESULT,
			fn RequestAccessAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::radios::RadioAccessStatus>) -> ::w::HRESULT
		}}
		RT_CLASS!(Radio: ::rt::gen::windows::devices::radios::IRadio);
		DEFINE_IID!(IID_IRadio, 622926047, 45886, 16746, 135, 95, 28, 243, 138, 226, 216, 62);
		RT_INTERFACE!{interface IRadio(IRadioVtbl): IInspectable(IInspectableVtbl) [IID_IRadio] {
			fn SetStateAsync(&mut self, value: ::rt::gen::windows::devices::radios::RadioState, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::radios::RadioAccessStatus>) -> ::w::HRESULT,
			fn add_StateChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::radios::Radio, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StateChanged(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn get_State(&mut self, out: *mut ::rt::gen::windows::devices::radios::RadioState) -> ::w::HRESULT,
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Kind(&mut self, out: *mut ::rt::gen::windows::devices::radios::RadioKind) -> ::w::HRESULT
		}}
} // Windows.Devices.Radios
pub mod sensors { // Windows.Devices.Sensors
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum MagnetometerAccuracy: i32 {
			Unknown (MagnetometerAccuracy_Unknown) = 0, Unreliable (MagnetometerAccuracy_Unreliable) = 1, Approximate (MagnetometerAccuracy_Approximate) = 2, High (MagnetometerAccuracy_High) = 3,
		}}
		RT_ENUM! { enum ActivityType: i32 {
			Unknown (ActivityType_Unknown) = 0, Idle (ActivityType_Idle) = 1, Stationary (ActivityType_Stationary) = 2, Fidgeting (ActivityType_Fidgeting) = 3, Walking (ActivityType_Walking) = 4, Running (ActivityType_Running) = 5, InVehicle (ActivityType_InVehicle) = 6, Biking (ActivityType_Biking) = 7,
		}}
		RT_ENUM! { enum ActivitySensorReadingConfidence: i32 {
			High (ActivitySensorReadingConfidence_High) = 0, Low (ActivitySensorReadingConfidence_Low) = 1,
		}}
		RT_ENUM! { enum SensorReadingType: i32 {
			Absolute (SensorReadingType_Absolute) = 0, Relative (SensorReadingType_Relative) = 1,
		}}
		RT_ENUM! { enum SensorType: i32 {
			Accelerometer (SensorType_Accelerometer) = 0, ActivitySensor (SensorType_ActivitySensor) = 1, Barometer (SensorType_Barometer) = 2, Compass (SensorType_Compass) = 3, CustomSensor (SensorType_CustomSensor) = 4, Gyroscope (SensorType_Gyroscope) = 5, ProximitySensor (SensorType_ProximitySensor) = 6, Inclinometer (SensorType_Inclinometer) = 7, LightSensor (SensorType_LightSensor) = 8, OrientationSensor (SensorType_OrientationSensor) = 9, Pedometer (SensorType_Pedometer) = 10, RelativeInclinometer (SensorType_RelativeInclinometer) = 11, RelativeOrientationSensor (SensorType_RelativeOrientationSensor) = 12, SimpleOrientationSensor (SensorType_SimpleOrientationSensor) = 13,
		}}
		DEFINE_IID!(IID_ISensorDataThreshold, 1423633505, 65099, 19975, 178, 96, 58, 76, 223, 190, 57, 110);
		RT_INTERFACE!{interface ISensorDataThreshold(ISensorDataThresholdVtbl): IInspectable(IInspectableVtbl) [IID_ISensorDataThreshold] {
			
		}}
		DEFINE_IID!(IID_ISensorDataThresholdTriggerDetails, 2433151415, 59533, 18609, 188, 144, 97, 156, 123, 52, 147, 145);
		RT_INTERFACE!{interface ISensorDataThresholdTriggerDetails(ISensorDataThresholdTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_ISensorDataThresholdTriggerDetails] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SensorType(&mut self, out: *mut ::rt::gen::windows::devices::sensors::SensorType) -> ::w::HRESULT
		}}
		RT_CLASS!(SensorDataThresholdTriggerDetails: ::rt::gen::windows::devices::sensors::ISensorDataThresholdTriggerDetails);
		DEFINE_IID!(IID_IAccelerometerDeviceId, 2125227177, 38869, 17517, 171, 90, 145, 125, 249, 185, 106, 44);
		RT_INTERFACE!{interface IAccelerometerDeviceId(IAccelerometerDeviceIdVtbl): IInspectable(IInspectableVtbl) [IID_IAccelerometerDeviceId] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAccelerometerStatics, 2783087476, 23175, 18989, 190, 204, 15, 144, 110, 160, 97, 221);
		RT_INTERFACE!{interface IAccelerometerStatics(IAccelerometerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IAccelerometerStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::Accelerometer) -> ::w::HRESULT
		}}
		RT_CLASS!(Accelerometer: ::rt::gen::windows::devices::sensors::IAccelerometer);
		DEFINE_IID!(IID_IAccelerometer, 3742909768, 10001, 19879, 128, 152, 75, 130, 32, 93, 60, 125);
		RT_INTERFACE!{interface IAccelerometer(IAccelerometerVtbl): IInspectable(IInspectableVtbl) [IID_IAccelerometer] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::AccelerometerReading) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Accelerometer, &::rt::gen::windows::devices::sensors::AccelerometerReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_Shaken(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Accelerometer, &::rt::gen::windows::devices::sensors::AccelerometerShakenEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Shaken(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(AccelerometerReading: ::rt::gen::windows::devices::sensors::IAccelerometerReading);
		RT_CLASS!(AccelerometerReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IAccelerometerReadingChangedEventArgs);
		RT_CLASS!(AccelerometerShakenEventArgs: ::rt::gen::windows::devices::sensors::IAccelerometerShakenEventArgs);
		DEFINE_IID!(IID_IAccelerometer2, 3908080366, 18788, 16410, 182, 2, 34, 13, 113, 83, 198, 10);
		RT_INTERFACE!{interface IAccelerometer2(IAccelerometer2Vtbl): IInspectable(IInspectableVtbl) [IID_IAccelerometer2] {
			fn put_ReadingTransform(&mut self, value: ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingTransform(&mut self, out: *mut ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAccelerometer3, 2279604778, 60800, 18923, 191, 138, 164, 234, 49, 229, 205, 132);
		RT_INTERFACE!{interface IAccelerometer3(IAccelerometer3Vtbl): IInspectable(IInspectableVtbl) [IID_IAccelerometer3] {
			fn put_ReportLatency(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportLatency(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_MaxBatchSize(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAccelerometerReading, 3120462539, 54097, 16559, 139, 182, 122, 169, 174, 100, 31, 183);
		RT_INTERFACE!{interface IAccelerometerReading(IAccelerometerReadingVtbl): IInspectable(IInspectableVtbl) [IID_IAccelerometerReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_AccelerationX(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_AccelerationY(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_AccelerationZ(&mut self, out: *mut f64) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAccelerometerReadingChangedEventArgs, 9815643, 46764, 18266, 159, 68, 139, 50, 211, 90, 63, 37);
		RT_INTERFACE!{interface IAccelerometerReadingChangedEventArgs(IAccelerometerReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAccelerometerReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::AccelerometerReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAccelerometerShakenEventArgs, 2516517329, 18984, 20277, 152, 232, 129, 120, 170, 228, 8, 74);
		RT_INTERFACE!{interface IAccelerometerShakenEventArgs(IAccelerometerShakenEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAccelerometerShakenEventArgs] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IInclinometerDeviceId, 32053634, 16895, 17414, 174, 131, 98, 33, 15, 241, 111, 227);
		RT_INTERFACE!{interface IInclinometerDeviceId(IInclinometerDeviceIdVtbl): IInspectable(IInspectableVtbl) [IID_IInclinometerDeviceId] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IInclinometerStatics, 4063151441, 39984, 17722, 139, 73, 60, 62, 235, 51, 203, 97);
		RT_INTERFACE!{interface IInclinometerStatics(IInclinometerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IInclinometerStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::Inclinometer) -> ::w::HRESULT
		}}
		RT_CLASS!(Inclinometer: ::rt::gen::windows::devices::sensors::IInclinometer);
		DEFINE_IID!(IID_IInclinometerStatics2, 71276405, 27166, 18844, 134, 224, 99, 140, 26, 134, 75, 0);
		RT_INTERFACE!{interface IInclinometerStatics2(IInclinometerStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IInclinometerStatics2] {
			fn GetDefaultForRelativeReadings(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::Inclinometer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IInclinometer, 642304623, 8838, 16495, 145, 97, 240, 196, 189, 128, 110, 191);
		RT_INTERFACE!{interface IInclinometer(IInclinometerVtbl): IInspectable(IInspectableVtbl) [IID_IInclinometer] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::InclinometerReading) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Inclinometer, &::rt::gen::windows::devices::sensors::InclinometerReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(InclinometerReading: ::rt::gen::windows::devices::sensors::IInclinometerReading);
		RT_CLASS!(InclinometerReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IInclinometerReadingChangedEventArgs);
		DEFINE_IID!(IID_IInclinometer2, 43987859, 10418, 17912, 187, 22, 97, 232, 106, 127, 174, 110);
		RT_INTERFACE!{interface IInclinometer2(IInclinometer2Vtbl): IInspectable(IInspectableVtbl) [IID_IInclinometer2] {
			fn put_ReadingTransform(&mut self, value: ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingTransform(&mut self, out: *mut ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingType(&mut self, out: *mut ::rt::gen::windows::devices::sensors::SensorReadingType) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IInclinometerReading, 2672095317, 46838, 18815, 177, 39, 26, 119, 94, 80, 20, 88);
		RT_INTERFACE!{interface IInclinometerReading(IInclinometerReadingVtbl): IInspectable(IInspectableVtbl) [IID_IInclinometerReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_PitchDegrees(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_RollDegrees(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_YawDegrees(&mut self, out: *mut f32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IInclinometerReadingYawAccuracy, 3025397888, 8163, 18822, 162, 87, 230, 236, 226, 114, 57, 73);
		RT_INTERFACE!{interface IInclinometerReadingYawAccuracy(IInclinometerReadingYawAccuracyVtbl): IInspectable(IInspectableVtbl) [IID_IInclinometerReadingYawAccuracy] {
			fn get_YawAccuracy(&mut self, out: *mut ::rt::gen::windows::devices::sensors::MagnetometerAccuracy) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IInclinometerReadingChangedEventArgs, 1256791489, 59371, 18744, 133, 17, 174, 13, 107, 68, 4, 56);
		RT_INTERFACE!{interface IInclinometerReadingChangedEventArgs(IInclinometerReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IInclinometerReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::InclinometerReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGyrometerDeviceId, 518383992, 35234, 17013, 158, 149, 113, 38, 244, 112, 135, 96);
		RT_INTERFACE!{interface IGyrometerDeviceId(IGyrometerDeviceIdVtbl): IInspectable(IInspectableVtbl) [IID_IGyrometerDeviceId] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGyrometerStatics, 2209802185, 58525, 19257, 134, 230, 205, 85, 75, 228, 197, 193);
		RT_INTERFACE!{interface IGyrometerStatics(IGyrometerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IGyrometerStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::Gyrometer) -> ::w::HRESULT
		}}
		RT_CLASS!(Gyrometer: ::rt::gen::windows::devices::sensors::IGyrometer);
		DEFINE_IID!(IID_IGyrometer, 4256803268, 33969, 19618, 151, 99, 155, 88, 149, 6, 199, 12);
		RT_INTERFACE!{interface IGyrometer(IGyrometerVtbl): IInspectable(IInspectableVtbl) [IID_IGyrometer] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::GyrometerReading) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Gyrometer, &::rt::gen::windows::devices::sensors::GyrometerReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(GyrometerReading: ::rt::gen::windows::devices::sensors::IGyrometerReading);
		RT_CLASS!(GyrometerReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IGyrometerReadingChangedEventArgs);
		DEFINE_IID!(IID_IGyrometer2, 1675568195, 36072, 16835, 172, 68, 134, 152, 129, 11, 85, 127);
		RT_INTERFACE!{interface IGyrometer2(IGyrometer2Vtbl): IInspectable(IInspectableVtbl) [IID_IGyrometer2] {
			fn put_ReadingTransform(&mut self, value: ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingTransform(&mut self, out: *mut ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGyrometerReading, 3017203292, 7908, 17775, 157, 231, 226, 73, 59, 92, 142, 3);
		RT_INTERFACE!{interface IGyrometerReading(IGyrometerReadingVtbl): IInspectable(IInspectableVtbl) [IID_IGyrometerReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_AngularVelocityX(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_AngularVelocityY(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_AngularVelocityZ(&mut self, out: *mut f64) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IGyrometerReadingChangedEventArgs, 266279061, 28574, 17102, 141, 88, 56, 140, 10, 184, 53, 109);
		RT_INTERFACE!{interface IGyrometerReadingChangedEventArgs(IGyrometerReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IGyrometerReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::GyrometerReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICompassDeviceId, 3514944041, 45189, 19229, 135, 10, 79, 245, 123, 167, 79, 212);
		RT_INTERFACE!{interface ICompassDeviceId(ICompassDeviceIdVtbl): IInspectable(IInspectableVtbl) [IID_ICompassDeviceId] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICompassStatics, 2596050911, 22252, 19493, 181, 77, 64, 166, 139, 181, 178, 105);
		RT_INTERFACE!{interface ICompassStatics(ICompassStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ICompassStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::Compass) -> ::w::HRESULT
		}}
		RT_CLASS!(Compass: ::rt::gen::windows::devices::sensors::ICompass);
		DEFINE_IID!(IID_ICompass, 691010196, 6981, 16444, 186, 6, 177, 6, 219, 166, 154, 100);
		RT_INTERFACE!{interface ICompass(ICompassVtbl): IInspectable(IInspectableVtbl) [IID_ICompass] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::CompassReading) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Compass, &::rt::gen::windows::devices::sensors::CompassReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(CompassReading: ::rt::gen::windows::devices::sensors::ICompassReading);
		RT_CLASS!(CompassReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::ICompassReadingChangedEventArgs);
		DEFINE_IID!(IID_ICompass2, 921857289, 51159, 17231, 180, 97, 151, 157, 223, 194, 50, 47);
		RT_INTERFACE!{interface ICompass2(ICompass2Vtbl): IInspectable(IInspectableVtbl) [IID_ICompass2] {
			fn put_ReadingTransform(&mut self, value: ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingTransform(&mut self, out: *mut ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICompassReading, 2190545192, 20797, 19913, 183, 129, 94, 237, 251, 240, 45, 12);
		RT_INTERFACE!{interface ICompassReading(ICompassReadingVtbl): IInspectable(IInspectableVtbl) [IID_ICompassReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_HeadingMagneticNorth(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_HeadingTrueNorth(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<f64>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICompassReadingHeadingAccuracy, 3881907534, 35089, 16631, 158, 22, 110, 204, 125, 174, 197, 222);
		RT_INTERFACE!{interface ICompassReadingHeadingAccuracy(ICompassReadingHeadingAccuracyVtbl): IInspectable(IInspectableVtbl) [IID_ICompassReadingHeadingAccuracy] {
			fn get_HeadingAccuracy(&mut self, out: *mut ::rt::gen::windows::devices::sensors::MagnetometerAccuracy) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICompassReadingChangedEventArgs, 2400537008, 59580, 19582, 176, 9, 78, 65, 223, 19, 112, 114);
		RT_INTERFACE!{interface ICompassReadingChangedEventArgs(ICompassReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ICompassReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::CompassReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILightSensorDeviceId, 2146322936, 2811, 20305, 135, 240, 108, 38, 55, 92, 233, 79);
		RT_INTERFACE!{interface ILightSensorDeviceId(ILightSensorDeviceIdVtbl): IInspectable(IInspectableVtbl) [IID_ILightSensorDeviceId] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILightSensorStatics, 1172016260, 50088, 18206, 154, 83, 100, 87, 250, 216, 124, 14);
		RT_INTERFACE!{interface ILightSensorStatics(ILightSensorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ILightSensorStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::LightSensor) -> ::w::HRESULT
		}}
		RT_CLASS!(LightSensor: ::rt::gen::windows::devices::sensors::ILightSensor);
		DEFINE_IID!(IID_ILightSensor, 4165732120, 3156, 18350, 146, 46, 120, 159, 87, 251, 3, 160);
		RT_INTERFACE!{interface ILightSensor(ILightSensorVtbl): IInspectable(IInspectableVtbl) [IID_ILightSensor] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::LightSensorReading) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::LightSensor, &::rt::gen::windows::devices::sensors::LightSensorReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(LightSensorReading: ::rt::gen::windows::devices::sensors::ILightSensorReading);
		RT_CLASS!(LightSensorReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::ILightSensorReadingChangedEventArgs);
		DEFINE_IID!(IID_ILightSensorReading, 4292829952, 8828, 19755, 179, 2, 252, 1, 66, 72, 92, 104);
		RT_INTERFACE!{interface ILightSensorReading(ILightSensorReadingVtbl): IInspectable(IInspectableVtbl) [IID_ILightSensorReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_IlluminanceInLux(&mut self, out: *mut f32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILightSensorReadingChangedEventArgs, 2745365711, 9611, 16908, 184, 171, 142, 221, 96, 30, 207, 80);
		RT_INTERFACE!{interface ILightSensorReadingChangedEventArgs(ILightSensorReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ILightSensorReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::LightSensorReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISensorRotationMatrix, 171792999, 8948, 17298, 149, 56, 101, 208, 189, 6, 74, 166);
		RT_INTERFACE!{interface ISensorRotationMatrix(ISensorRotationMatrixVtbl): IInspectable(IInspectableVtbl) [IID_ISensorRotationMatrix] {
			fn get_M11(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_M12(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_M13(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_M21(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_M22(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_M23(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_M31(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_M32(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_M33(&mut self, out: *mut f32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISensorQuaternion, 3385182247, 50972, 18151, 157, 163, 54, 161, 147, 178, 50, 188);
		RT_INTERFACE!{interface ISensorQuaternion(ISensorQuaternionVtbl): IInspectable(IInspectableVtbl) [IID_ISensorQuaternion] {
			fn get_W(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_X(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_Y(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_Z(&mut self, out: *mut f32) -> ::w::HRESULT
		}}
		RT_CLASS!(SensorRotationMatrix: ::rt::gen::windows::devices::sensors::ISensorRotationMatrix);
		RT_CLASS!(SensorQuaternion: ::rt::gen::windows::devices::sensors::ISensorQuaternion);
		DEFINE_IID!(IID_IOrientationSensorDeviceId, 1516877384, 19497, 18924, 178, 143, 234, 29, 17, 123, 102, 240);
		RT_INTERFACE!{interface IOrientationSensorDeviceId(IOrientationSensorDeviceIdVtbl): IInspectable(IInspectableVtbl) [IID_IOrientationSensorDeviceId] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IOrientationSensorStatics, 284133138, 64332, 17034, 137, 139, 39, 101, 228, 9, 230, 105);
		RT_INTERFACE!{interface IOrientationSensorStatics(IOrientationSensorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IOrientationSensorStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::OrientationSensor) -> ::w::HRESULT
		}}
		RT_CLASS!(OrientationSensor: ::rt::gen::windows::devices::sensors::IOrientationSensor);
		DEFINE_IID!(IID_IOrientationSensorStatics2, 1507462411, 54282, 19569, 146, 118, 138, 39, 42, 10, 102, 25);
		RT_INTERFACE!{interface IOrientationSensorStatics2(IOrientationSensorStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IOrientationSensorStatics2] {
			fn GetDefaultForRelativeReadings(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::OrientationSensor) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IOrientationSensor, 1580549685, 53099, 19555, 171, 216, 16, 37, 43, 11, 246, 236);
		RT_INTERFACE!{interface IOrientationSensor(IOrientationSensorVtbl): IInspectable(IInspectableVtbl) [IID_IOrientationSensor] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::OrientationSensorReading) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::OrientationSensor, &::rt::gen::windows::devices::sensors::OrientationSensorReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(OrientationSensorReading: ::rt::gen::windows::devices::sensors::IOrientationSensorReading);
		RT_CLASS!(OrientationSensorReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IOrientationSensorReadingChangedEventArgs);
		DEFINE_IID!(IID_IOrientationSensor2, 227691769, 12063, 18889, 128, 66, 74, 24, 19, 214, 119, 96);
		RT_INTERFACE!{interface IOrientationSensor2(IOrientationSensor2Vtbl): IInspectable(IInspectableVtbl) [IID_IOrientationSensor2] {
			fn put_ReadingTransform(&mut self, value: ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingTransform(&mut self, out: *mut ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingType(&mut self, out: *mut ::rt::gen::windows::devices::sensors::SensorReadingType) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IOrientationSensorReading, 1196870035, 26005, 18583, 188, 198, 213, 55, 238, 117, 117, 100);
		RT_INTERFACE!{interface IOrientationSensorReading(IOrientationSensorReadingVtbl): IInspectable(IInspectableVtbl) [IID_IOrientationSensorReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_RotationMatrix(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::SensorRotationMatrix) -> ::w::HRESULT,
			fn get_Quaternion(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::SensorQuaternion) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IOrientationSensorReadingYawAccuracy, 3517749284, 16218, 18850, 188, 123, 17, 128, 188, 56, 205, 43);
		RT_INTERFACE!{interface IOrientationSensorReadingYawAccuracy(IOrientationSensorReadingYawAccuracyVtbl): IInspectable(IInspectableVtbl) [IID_IOrientationSensorReadingYawAccuracy] {
			fn get_YawAccuracy(&mut self, out: *mut ::rt::gen::windows::devices::sensors::MagnetometerAccuracy) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IOrientationSensorReadingChangedEventArgs, 19665286, 50106, 18108, 174, 101, 122, 152, 153, 108, 191, 184);
		RT_INTERFACE!{interface IOrientationSensorReadingChangedEventArgs(IOrientationSensorReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IOrientationSensorReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::OrientationSensorReading) -> ::w::HRESULT
		}}
		RT_ENUM! { enum SimpleOrientation: i32 {
			NotRotated (SimpleOrientation_NotRotated) = 0, Rotated90DegreesCounterclockwise (SimpleOrientation_Rotated90DegreesCounterclockwise) = 1, Rotated180DegreesCounterclockwise (SimpleOrientation_Rotated180DegreesCounterclockwise) = 2, Rotated270DegreesCounterclockwise (SimpleOrientation_Rotated270DegreesCounterclockwise) = 3, Faceup (SimpleOrientation_Faceup) = 4, Facedown (SimpleOrientation_Facedown) = 5,
		}}
		DEFINE_IID!(IID_ISimpleOrientationSensorDeviceId, 4223666891, 15222, 16886, 128, 145, 48, 239, 230, 70, 211, 207);
		RT_INTERFACE!{interface ISimpleOrientationSensorDeviceId(ISimpleOrientationSensorDeviceIdVtbl): IInspectable(IInspectableVtbl) [IID_ISimpleOrientationSensorDeviceId] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISimpleOrientationSensorStatics, 1928136303, 28842, 16582, 155, 27, 52, 51, 247, 69, 155, 78);
		RT_INTERFACE!{interface ISimpleOrientationSensorStatics(ISimpleOrientationSensorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISimpleOrientationSensorStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::SimpleOrientationSensor) -> ::w::HRESULT
		}}
		RT_CLASS!(SimpleOrientationSensor: ::rt::gen::windows::devices::sensors::ISimpleOrientationSensor);
		DEFINE_IID!(IID_ISimpleOrientationSensor, 1609906262, 8522, 19950, 163, 249, 97, 111, 26, 176, 111, 253);
		RT_INTERFACE!{interface ISimpleOrientationSensor(ISimpleOrientationSensorVtbl): IInspectable(IInspectableVtbl) [IID_ISimpleOrientationSensor] {
			fn GetCurrentOrientation(&mut self, out: *mut ::rt::gen::windows::devices::sensors::SimpleOrientation) -> ::w::HRESULT,
			fn add_OrientationChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::SimpleOrientationSensor, &::rt::gen::windows::devices::sensors::SimpleOrientationSensorOrientationChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_OrientationChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(SimpleOrientationSensorOrientationChangedEventArgs: ::rt::gen::windows::devices::sensors::ISimpleOrientationSensorOrientationChangedEventArgs);
		DEFINE_IID!(IID_ISimpleOrientationSensor2, 2725750680, 34928, 17726, 139, 214, 184, 245, 216, 215, 148, 27);
		RT_INTERFACE!{interface ISimpleOrientationSensor2(ISimpleOrientationSensor2Vtbl): IInspectable(IInspectableVtbl) [IID_ISimpleOrientationSensor2] {
			fn put_ReadingTransform(&mut self, value: ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingTransform(&mut self, out: *mut ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISimpleOrientationSensorOrientationChangedEventArgs, 3168126560, 9172, 19276, 162, 46, 186, 129, 173, 224, 198, 1);
		RT_INTERFACE!{interface ISimpleOrientationSensorOrientationChangedEventArgs(ISimpleOrientationSensorOrientationChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ISimpleOrientationSensorOrientationChangedEventArgs] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_Orientation(&mut self, out: *mut ::rt::gen::windows::devices::sensors::SimpleOrientation) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagnetometerDeviceId, 1488230594, 32331, 16460, 159, 197, 93, 232, 180, 14, 186, 227);
		RT_INTERFACE!{interface IMagnetometerDeviceId(IMagnetometerDeviceIdVtbl): IInspectable(IInspectableVtbl) [IID_IMagnetometerDeviceId] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagnetometerStatics, 2235327692, 1688, 19930, 166, 223, 156, 185, 204, 74, 180, 10);
		RT_INTERFACE!{interface IMagnetometerStatics(IMagnetometerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IMagnetometerStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::Magnetometer) -> ::w::HRESULT
		}}
		RT_CLASS!(Magnetometer: ::rt::gen::windows::devices::sensors::IMagnetometer);
		DEFINE_IID!(IID_IMagnetometer, 1213162094, 54217, 16657, 179, 246, 44, 241, 250, 164, 24, 213);
		RT_INTERFACE!{interface IMagnetometer(IMagnetometerVtbl): IInspectable(IInspectableVtbl) [IID_IMagnetometer] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::MagnetometerReading) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Magnetometer, &::rt::gen::windows::devices::sensors::MagnetometerReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(MagnetometerReading: ::rt::gen::windows::devices::sensors::IMagnetometerReading);
		RT_CLASS!(MagnetometerReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IMagnetometerReadingChangedEventArgs);
		DEFINE_IID!(IID_IMagnetometer2, 3026545797, 9974, 17483, 169, 226, 162, 63, 150, 108, 211, 104);
		RT_INTERFACE!{interface IMagnetometer2(IMagnetometer2Vtbl): IInspectable(IInspectableVtbl) [IID_IMagnetometer2] {
			fn put_ReadingTransform(&mut self, value: ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT,
			fn get_ReadingTransform(&mut self, out: *mut ::rt::gen::windows::graphics::display::DisplayOrientations) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagnetometerReading, 204260365, 60413, 20060, 187, 17, 175, 194, 155, 60, 174, 97);
		RT_INTERFACE!{interface IMagnetometerReading(IMagnetometerReadingVtbl): IInspectable(IInspectableVtbl) [IID_IMagnetometerReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_MagneticFieldX(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_MagneticFieldY(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_MagneticFieldZ(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn get_DirectionalAccuracy(&mut self, out: *mut ::rt::gen::windows::devices::sensors::MagnetometerAccuracy) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMagnetometerReadingChangedEventArgs, 401270898, 11961, 20199, 138, 208, 49, 39, 83, 125, 148, 155);
		RT_INTERFACE!{interface IMagnetometerReadingChangedEventArgs(IMagnetometerReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMagnetometerReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::MagnetometerReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IActivitySensorStatics, 2803764893, 61067, 17873, 178, 91, 8, 204, 13, 249, 42, 182);
		RT_INTERFACE!{interface IActivitySensorStatics(IActivitySensorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IActivitySensorStatics] {
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sensors::ActivitySensor>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sensors::ActivitySensor>) -> ::w::HRESULT,
			fn GetSystemHistoryAsync(&mut self, fromTime: ::rt::gen::windows::foundation::DateTime, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sensors::ActivitySensorReading>>) -> ::w::HRESULT,
			fn GetSystemHistoryWithDurationAsync(&mut self, fromTime: ::rt::gen::windows::foundation::DateTime, duration: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sensors::ActivitySensorReading>>) -> ::w::HRESULT
		}}
		RT_CLASS!(ActivitySensor: ::rt::gen::windows::devices::sensors::IActivitySensor);
		RT_CLASS!(ActivitySensorReading: ::rt::gen::windows::devices::sensors::IActivitySensorReading);
		DEFINE_IID!(IID_IActivitySensor, 3447350028, 64351, 18667, 176, 155, 162, 112, 141, 28, 97, 239);
		RT_INTERFACE!{interface IActivitySensor(IActivitySensorVtbl): IInspectable(IInspectableVtbl) [IID_IActivitySensor] {
			fn GetCurrentReadingAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sensors::ActivitySensorReading>) -> ::w::HRESULT,
			fn get_SubscribedActivities(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<::rt::gen::windows::devices::sensors::ActivityType>) -> ::w::HRESULT,
			fn get_PowerInMilliwatts(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SupportedActivities(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<::rt::gen::windows::devices::sensors::ActivityType>) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::ActivitySensor, &::rt::gen::windows::devices::sensors::ActivitySensorReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(ActivitySensorReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IActivitySensorReadingChangedEventArgs);
		DEFINE_IID!(IID_IActivitySensorReading, 2232572566, 5234, 16546, 178, 174, 225, 239, 41, 34, 108, 120);
		RT_INTERFACE!{interface IActivitySensorReading(IActivitySensorReadingVtbl): IInspectable(IInspectableVtbl) [IID_IActivitySensorReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_Activity(&mut self, out: *mut ::rt::gen::windows::devices::sensors::ActivityType) -> ::w::HRESULT,
			fn get_Confidence(&mut self, out: *mut ::rt::gen::windows::devices::sensors::ActivitySensorReadingConfidence) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IActivitySensorReadingChangedEventArgs, 3728238359, 44726, 20167, 148, 106, 217, 204, 25, 185, 81, 236);
		RT_INTERFACE!{interface IActivitySensorReadingChangedEventArgs(IActivitySensorReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IActivitySensorReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::ActivitySensorReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IActivitySensorReadingChangeReport, 1329342741, 55611, 18365, 150, 10, 242, 15, 178, 243, 34, 185);
		RT_INTERFACE!{interface IActivitySensorReadingChangeReport(IActivitySensorReadingChangeReportVtbl): IInspectable(IInspectableVtbl) [IID_IActivitySensorReadingChangeReport] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::ActivitySensorReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IActivitySensorTriggerDetails, 748578322, 47562, 18039, 178, 99, 36, 50, 151, 247, 157, 58);
		RT_INTERFACE!{interface IActivitySensorTriggerDetails(IActivitySensorTriggerDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IActivitySensorTriggerDetails] {
			fn ReadReports(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sensors::ActivitySensorReadingChangeReport>) -> ::w::HRESULT
		}}
		RT_CLASS!(ActivitySensorReadingChangeReport: ::rt::gen::windows::devices::sensors::IActivitySensorReadingChangeReport);
		RT_CLASS!(ActivitySensorTriggerDetails: ::rt::gen::windows::devices::sensors::IActivitySensorTriggerDetails);
		DEFINE_IID!(IID_IBarometerStatics, 678110986, 739, 20358, 132, 252, 253, 216, 146, 181, 148, 15);
		RT_INTERFACE!{interface IBarometerStatics(IBarometerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IBarometerStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::Barometer) -> ::w::HRESULT
		}}
		RT_CLASS!(Barometer: ::rt::gen::windows::devices::sensors::IBarometer);
		DEFINE_IID!(IID_IBarometer, 2470737320, 30911, 17711, 176, 23, 240, 32, 156, 230, 218, 180);
		RT_INTERFACE!{interface IBarometer(IBarometerVtbl): IInspectable(IInspectableVtbl) [IID_IBarometer] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::BarometerReading) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Barometer, &::rt::gen::windows::devices::sensors::BarometerReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(BarometerReading: ::rt::gen::windows::devices::sensors::IBarometerReading);
		RT_CLASS!(BarometerReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IBarometerReadingChangedEventArgs);
		DEFINE_IID!(IID_IBarometerReading, 4122596070, 7670, 18970, 167, 173, 50, 29, 79, 93, 178, 71);
		RT_INTERFACE!{interface IBarometerReading(IBarometerReadingVtbl): IInspectable(IInspectableVtbl) [IID_IBarometerReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_StationPressureInHectopascals(&mut self, out: *mut f64) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IBarometerReadingChangedEventArgs, 1032098911, 891, 16463, 155, 187, 98, 50, 214, 149, 67, 195);
		RT_INTERFACE!{interface IBarometerReadingChangedEventArgs(IBarometerReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IBarometerReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::BarometerReading) -> ::w::HRESULT
		}}
		RT_ENUM! { enum PedometerStepKind: i32 {
			Unknown (PedometerStepKind_Unknown) = 0, Walking (PedometerStepKind_Walking) = 1, Running (PedometerStepKind_Running) = 2,
		}}
		DEFINE_IID!(IID_IPedometerReading, 575003892, 43233, 17199, 137, 106, 190, 13, 217, 176, 45, 36);
		RT_INTERFACE!{interface IPedometerReading(IPedometerReadingVtbl): IInspectable(IInspectableVtbl) [IID_IPedometerReading] {
			fn get_StepKind(&mut self, out: *mut ::rt::gen::windows::devices::sensors::PedometerStepKind) -> ::w::HRESULT,
			fn get_CumulativeSteps(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_CumulativeStepsDuration(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPedometerReadingChangedEventArgs, 4166378622, 43964, 17494, 134, 168, 37, 207, 43, 51, 55, 66);
		RT_INTERFACE!{interface IPedometerReadingChangedEventArgs(IPedometerReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPedometerReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::PedometerReading) -> ::w::HRESULT
		}}
		RT_CLASS!(PedometerReading: ::rt::gen::windows::devices::sensors::IPedometerReading);
		DEFINE_IID!(IID_IPedometerStatics, 2191002159, 16515, 19963, 180, 17, 147, 142, 160, 244, 185, 70);
		RT_INTERFACE!{interface IPedometerStatics(IPedometerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPedometerStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sensors::Pedometer>) -> ::w::HRESULT,
			fn GetDefaultAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sensors::Pedometer>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetSystemHistoryAsync(&mut self, fromTime: ::rt::gen::windows::foundation::DateTime, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sensors::PedometerReading>>) -> ::w::HRESULT,
			fn GetSystemHistoryWithDurationAsync(&mut self, fromTime: ::rt::gen::windows::foundation::DateTime, duration: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sensors::PedometerReading>>) -> ::w::HRESULT
		}}
		RT_CLASS!(Pedometer: ::rt::gen::windows::devices::sensors::IPedometer);
		DEFINE_IID!(IID_IPedometerStatics2, 2046150331, 52750, 16691, 180, 126, 134, 39, 234, 114, 246, 119);
		RT_INTERFACE!{interface IPedometerStatics2(IPedometerStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IPedometerStatics2] {
			fn GetReadingsFromTriggerDetails(&mut self, triggerDetails: *mut ::rt::gen::windows::devices::sensors::SensorDataThresholdTriggerDetails, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sensors::PedometerReading>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPedometer2, 3852732127, 11137, 19165, 178, 255, 119, 171, 108, 152, 186, 25);
		RT_INTERFACE!{interface IPedometer2(IPedometer2Vtbl): IInspectable(IInspectableVtbl) [IID_IPedometer2] {
			fn GetCurrentReadings(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<::rt::gen::windows::devices::sensors::PedometerStepKind, &::rt::gen::windows::devices::sensors::PedometerReading>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPedometer, 2585657661, 15768, 17912, 137, 32, 142, 78, 202, 202, 95, 151);
		RT_INTERFACE!{interface IPedometer(IPedometerVtbl): IInspectable(IInspectableVtbl) [IID_IPedometer] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_PowerInMilliwatts(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Pedometer, &::rt::gen::windows::devices::sensors::PedometerReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(PedometerReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IPedometerReadingChangedEventArgs);
		DEFINE_IID!(IID_IPedometerDataThresholdFactory, 3417149264, 31316, 18027, 144, 16, 119, 161, 98, 252, 165, 215);
		RT_INTERFACE!{interface IPedometerDataThresholdFactory(IPedometerDataThresholdFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IPedometerDataThresholdFactory] {
			fn Create(&mut self, sensor: *mut ::rt::gen::windows::devices::sensors::Pedometer, stepGoal: i32, out: *mut *mut ::rt::gen::windows::devices::sensors::PedometerDataThreshold) -> ::w::HRESULT
		}}
		RT_CLASS!(PedometerDataThreshold: ::rt::gen::windows::devices::sensors::ISensorDataThreshold);
		DEFINE_IID!(IID_IProximitySensorStatics, 689464905, 25193, 20055, 165, 173, 130, 190, 128, 129, 51, 146);
		RT_INTERFACE!{interface IProximitySensorStatics(IProximitySensorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IProximitySensorStatics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromId(&mut self, sensorId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::sensors::ProximitySensor) -> ::w::HRESULT
		}}
		RT_CLASS!(ProximitySensor: ::rt::gen::windows::devices::sensors::IProximitySensor);
		DEFINE_IID!(IID_IProximitySensor, 1421899448, 60667, 18756, 185, 40, 116, 252, 80, 77, 71, 238);
		RT_INTERFACE!{interface IProximitySensor(IProximitySensorVtbl): IInspectable(IInspectableVtbl) [IID_IProximitySensor] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_MaxDistanceInMillimeters(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u32>) -> ::w::HRESULT,
			fn get_MinDistanceInMillimeters(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u32>) -> ::w::HRESULT,
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::ProximitySensorReading) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::ProximitySensor, &::rt::gen::windows::devices::sensors::ProximitySensorReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn CreateDisplayOnOffController(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::ProximitySensorDisplayOnOffController) -> ::w::HRESULT
		}}
		RT_CLASS!(ProximitySensorReading: ::rt::gen::windows::devices::sensors::IProximitySensorReading);
		RT_CLASS!(ProximitySensorReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IProximitySensorReadingChangedEventArgs);
		RT_CLASS!(ProximitySensorDisplayOnOffController: ::rt::gen::windows::foundation::IClosable);
		DEFINE_IID!(IID_IProximitySensorReadingChangedEventArgs, 3485660006, 50152, 16637, 140, 195, 103, 226, 137, 0, 73, 56);
		RT_INTERFACE!{interface IProximitySensorReadingChangedEventArgs(IProximitySensorReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IProximitySensorReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::ProximitySensorReading) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IProximitySensorReading, 1898089817, 4909, 19807, 143, 249, 47, 13, 184, 117, 28, 237);
		RT_INTERFACE!{interface IProximitySensorReading(IProximitySensorReadingVtbl): IInspectable(IInspectableVtbl) [IID_IProximitySensorReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_IsDetected(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_DistanceInMillimeters(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u32>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IProximitySensorDataThresholdFactory, 2421866785, 27943, 19155, 157, 181, 100, 103, 242, 165, 173, 157);
		RT_INTERFACE!{interface IProximitySensorDataThresholdFactory(IProximitySensorDataThresholdFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IProximitySensorDataThresholdFactory] {
			fn Create(&mut self, sensor: *mut ::rt::gen::windows::devices::sensors::ProximitySensor, out: *mut *mut ::rt::gen::windows::devices::sensors::ProximitySensorDataThreshold) -> ::w::HRESULT
		}}
		RT_CLASS!(ProximitySensorDataThreshold: ::rt::gen::windows::devices::sensors::ISensorDataThreshold);
		DEFINE_IID!(IID_IProximitySensorStatics2, 3421795246, 59850, 16943, 173, 103, 76, 61, 37, 223, 53, 12);
		RT_INTERFACE!{interface IProximitySensorStatics2(IProximitySensorStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IProximitySensorStatics2] {
			fn GetReadingsFromTriggerDetails(&mut self, triggerDetails: *mut ::rt::gen::windows::devices::sensors::SensorDataThresholdTriggerDetails, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::sensors::ProximitySensorReading>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAltimeterStatics, 2662651843, 58796, 18382, 142, 239, 211, 113, 129, 104, 192, 31);
		RT_INTERFACE!{interface IAltimeterStatics(IAltimeterStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IAltimeterStatics] {
			fn GetDefault(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::Altimeter) -> ::w::HRESULT
		}}
		RT_CLASS!(Altimeter: ::rt::gen::windows::devices::sensors::IAltimeter);
		DEFINE_IID!(IID_IAltimeter, 1928353789, 36612, 18929, 180, 167, 244, 227, 99, 183, 1, 162);
		RT_INTERFACE!{interface IAltimeter(IAltimeterVtbl): IInspectable(IInspectableVtbl) [IID_IAltimeter] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::AltimeterReading) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::Altimeter, &::rt::gen::windows::devices::sensors::AltimeterReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(AltimeterReading: ::rt::gen::windows::devices::sensors::IAltimeterReading);
		RT_CLASS!(AltimeterReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::IAltimeterReadingChangedEventArgs);
		DEFINE_IID!(IID_IAltimeterReading, 4226346867, 32606, 18632, 170, 26, 241, 243, 190, 252, 17, 68);
		RT_INTERFACE!{interface IAltimeterReading(IAltimeterReadingVtbl): IInspectable(IInspectableVtbl) [IID_IAltimeterReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_AltitudeChangeInMeters(&mut self, out: *mut f64) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAltimeterReadingChangedEventArgs, 1885982839, 17517, 18423, 153, 140, 235, 194, 59, 69, 228, 162);
		RT_INTERFACE!{interface IAltimeterReadingChangedEventArgs(IAltimeterReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IAltimeterReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::AltimeterReading) -> ::w::HRESULT
		}}
pub mod custom { // Windows.Devices.Sensors.Custom
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_ICustomSensorStatics, 2569032399, 62498, 19581, 131, 107, 231, 220, 116, 167, 18, 75);
		RT_INTERFACE!{interface ICustomSensorStatics(ICustomSensorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ICustomSensorStatics] {
			fn GetDeviceSelector(&mut self, interfaceId: ::w::GUID, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, sensorId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::sensors::custom::CustomSensor>) -> ::w::HRESULT
		}}
		RT_CLASS!(CustomSensor: ::rt::gen::windows::devices::sensors::custom::ICustomSensor);
		DEFINE_IID!(IID_ICustomSensor, 2704734637, 16436, 19277, 153, 221, 83, 26, 172, 100, 156, 9);
		RT_INTERFACE!{interface ICustomSensor(ICustomSensorVtbl): IInspectable(IInspectableVtbl) [IID_ICustomSensor] {
			fn GetCurrentReading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::custom::CustomSensorReading) -> ::w::HRESULT,
			fn get_MinimumReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ReportInterval(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ReportInterval(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn add_ReadingChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::sensors::custom::CustomSensor, &::rt::gen::windows::devices::sensors::custom::CustomSensorReadingChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ReadingChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(CustomSensorReading: ::rt::gen::windows::devices::sensors::custom::ICustomSensorReading);
		RT_CLASS!(CustomSensorReadingChangedEventArgs: ::rt::gen::windows::devices::sensors::custom::ICustomSensorReadingChangedEventArgs);
		DEFINE_IID!(IID_ICustomSensorReading, 1677741901, 17514, 17254, 168, 122, 95, 150, 50, 104, 236, 83);
		RT_INTERFACE!{interface ICustomSensorReading(ICustomSensorReadingVtbl): IInspectable(IInspectableVtbl) [IID_ICustomSensorReading] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<&str, &IInspectable>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICustomSensorReadingChangedEventArgs, 1797267491, 53245, 19649, 143, 240, 226, 24, 35, 215, 111, 204);
		RT_INTERFACE!{interface ICustomSensorReadingChangedEventArgs(ICustomSensorReadingChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ICustomSensorReadingChangedEventArgs] {
			fn get_Reading(&mut self, out: *mut *mut ::rt::gen::windows::devices::sensors::custom::CustomSensorReading) -> ::w::HRESULT
		}}
} // Windows.Devices.Sensors.Custom
} // Windows.Devices.Sensors
pub mod serialcommunication { // Windows.Devices.SerialCommunication
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum SerialParity: i32 {
			None (SerialParity_None) = 0, Odd (SerialParity_Odd) = 1, Even (SerialParity_Even) = 2, Mark (SerialParity_Mark) = 3, Space (SerialParity_Space) = 4,
		}}
		RT_ENUM! { enum SerialHandshake: i32 {
			None (SerialHandshake_None) = 0, RequestToSend (SerialHandshake_RequestToSend) = 1, XOnXOff (SerialHandshake_XOnXOff) = 2, RequestToSendXOnXOff (SerialHandshake_RequestToSendXOnXOff) = 3,
		}}
		RT_ENUM! { enum SerialStopBitCount: i32 {
			One (SerialStopBitCount_One) = 0, OnePointFive (SerialStopBitCount_OnePointFive) = 1, Two (SerialStopBitCount_Two) = 2,
		}}
		RT_ENUM! { enum SerialError: i32 {
			Frame (SerialError_Frame) = 0, BufferOverrun (SerialError_BufferOverrun) = 1, ReceiveFull (SerialError_ReceiveFull) = 2, ReceiveParity (SerialError_ReceiveParity) = 3, TransmitFull (SerialError_TransmitFull) = 4,
		}}
		RT_ENUM! { enum SerialPinChange: i32 {
			BreakSignal (SerialPinChange_BreakSignal) = 0, CarrierDetect (SerialPinChange_CarrierDetect) = 1, ClearToSend (SerialPinChange_ClearToSend) = 2, DataSetReady (SerialPinChange_DataSetReady) = 3, RingIndicator (SerialPinChange_RingIndicator) = 4,
		}}
		DEFINE_IID!(IID_ISerialDeviceStatics, 93080176, 2102, 18835, 174, 26, 182, 26, 227, 190, 5, 107);
		RT_INTERFACE!{interface ISerialDeviceStatics(ISerialDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISerialDeviceStatics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromPortName(&mut self, portName: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromUsbVidPid(&mut self, vendorId: u16, productId: u16, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::serialcommunication::SerialDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(SerialDevice: ::rt::gen::windows::devices::serialcommunication::ISerialDevice);
		RT_CLASS!(ErrorReceivedEventArgs: ::rt::gen::windows::devices::serialcommunication::IErrorReceivedEventArgs);
		RT_CLASS!(PinChangedEventArgs: ::rt::gen::windows::devices::serialcommunication::IPinChangedEventArgs);
		DEFINE_IID!(IID_IErrorReceivedEventArgs, 4240883545, 4739, 19850, 191, 223, 86, 107, 51, 221, 178, 143);
		RT_INTERFACE!{interface IErrorReceivedEventArgs(IErrorReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IErrorReceivedEventArgs] {
			fn get_Error(&mut self, out: *mut ::rt::gen::windows::devices::serialcommunication::SerialError) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPinChangedEventArgs, 2730433968, 64668, 17927, 147, 208, 250, 94, 131, 67, 238, 34);
		RT_INTERFACE!{interface IPinChangedEventArgs(IPinChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IPinChangedEventArgs] {
			fn get_PinChange(&mut self, out: *mut ::rt::gen::windows::devices::serialcommunication::SerialPinChange) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISerialDevice, 3783773382, 8720, 16719, 182, 90, 245, 85, 58, 3, 55, 42);
		RT_INTERFACE!{interface ISerialDevice(ISerialDeviceVtbl): IInspectable(IInspectableVtbl) [IID_ISerialDevice] {
			fn get_BaudRate(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_BaudRate(&mut self, value: u32) -> ::w::HRESULT,
			fn get_BreakSignalState(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_BreakSignalState(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_BytesReceived(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_CarrierDetectState(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_ClearToSendState(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_DataBits(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn put_DataBits(&mut self, value: u16) -> ::w::HRESULT,
			fn get_DataSetReadyState(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Handshake(&mut self, out: *mut ::rt::gen::windows::devices::serialcommunication::SerialHandshake) -> ::w::HRESULT,
			fn put_Handshake(&mut self, value: ::rt::gen::windows::devices::serialcommunication::SerialHandshake) -> ::w::HRESULT,
			fn get_IsDataTerminalReadyEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsDataTerminalReadyEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_IsRequestToSendEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsRequestToSendEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_Parity(&mut self, out: *mut ::rt::gen::windows::devices::serialcommunication::SerialParity) -> ::w::HRESULT,
			fn put_Parity(&mut self, value: ::rt::gen::windows::devices::serialcommunication::SerialParity) -> ::w::HRESULT,
			fn get_PortName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ReadTimeout(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn put_ReadTimeout(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_StopBits(&mut self, out: *mut ::rt::gen::windows::devices::serialcommunication::SerialStopBitCount) -> ::w::HRESULT,
			fn put_StopBits(&mut self, value: ::rt::gen::windows::devices::serialcommunication::SerialStopBitCount) -> ::w::HRESULT,
			fn get_UsbVendorId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_UsbProductId(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_WriteTimeout(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn put_WriteTimeout(&mut self, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_InputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IInputStream) -> ::w::HRESULT,
			fn get_OutputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IOutputStream) -> ::w::HRESULT,
			fn add_ErrorReceived(&mut self, reportHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::serialcommunication::SerialDevice, &::rt::gen::windows::devices::serialcommunication::ErrorReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ErrorReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_PinChanged(&mut self, reportHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::serialcommunication::SerialDevice, &::rt::gen::windows::devices::serialcommunication::PinChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_PinChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
} // Windows.Devices.SerialCommunication
pub mod smartcards { // Windows.Devices.SmartCards
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum SmartCardReaderKind: i32 {
			Any (SmartCardReaderKind_Any) = 0, Generic (SmartCardReaderKind_Generic) = 1, Tpm (SmartCardReaderKind_Tpm) = 2, Nfc (SmartCardReaderKind_Nfc) = 3, Uicc (SmartCardReaderKind_Uicc) = 4,
		}}
		RT_ENUM! { enum SmartCardReaderStatus: i32 {
			Disconnected (SmartCardReaderStatus_Disconnected) = 0, Ready (SmartCardReaderStatus_Ready) = 1, Exclusive (SmartCardReaderStatus_Exclusive) = 2,
		}}
		RT_ENUM! { enum SmartCardStatus: i32 {
			Disconnected (SmartCardStatus_Disconnected) = 0, Ready (SmartCardStatus_Ready) = 1, Shared (SmartCardStatus_Shared) = 2, Exclusive (SmartCardStatus_Exclusive) = 3, Unresponsive (SmartCardStatus_Unresponsive) = 4,
		}}
		RT_ENUM! { enum SmartCardPinCharacterPolicyOption: i32 {
			Allow (SmartCardPinCharacterPolicyOption_Allow) = 0, RequireAtLeastOne (SmartCardPinCharacterPolicyOption_RequireAtLeastOne) = 1, Disallow (SmartCardPinCharacterPolicyOption_Disallow) = 2,
		}}
		DEFINE_IID!(IID_ISmartCardReaderStatics, 272368865, 41418, 18674, 162, 129, 91, 111, 102, 154, 241, 7);
		RT_INTERFACE!{interface ISmartCardReaderStatics(ISmartCardReaderStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardReaderStatics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorWithKind(&mut self, kind: ::rt::gen::windows::devices::smartcards::SmartCardReaderKind, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::smartcards::SmartCardReader>) -> ::w::HRESULT
		}}
		RT_CLASS!(SmartCardReader: ::rt::gen::windows::devices::smartcards::ISmartCardReader);
		DEFINE_IID!(IID_ISmartCardReader, 276083936, 21698, 19952, 129, 122, 20, 193, 67, 120, 240, 108);
		RT_INTERFACE!{interface ISmartCardReader(ISmartCardReaderVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardReader] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Kind(&mut self, out: *mut ::rt::gen::windows::devices::smartcards::SmartCardReaderKind) -> ::w::HRESULT,
			fn GetStatusAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::smartcards::SmartCardReaderStatus>) -> ::w::HRESULT,
			fn FindAllCardsAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::smartcards::SmartCard>>) -> ::w::HRESULT,
			fn add_CardAdded(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::smartcards::SmartCardReader, &::rt::gen::windows::devices::smartcards::CardAddedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_CardAdded(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_CardRemoved(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::smartcards::SmartCardReader, &::rt::gen::windows::devices::smartcards::CardRemovedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_CardRemoved(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(SmartCard: ::rt::gen::windows::devices::smartcards::ISmartCard);
		RT_CLASS!(CardAddedEventArgs: ::rt::gen::windows::devices::smartcards::ICardAddedEventArgs);
		RT_CLASS!(CardRemovedEventArgs: ::rt::gen::windows::devices::smartcards::ICardRemovedEventArgs);
		DEFINE_IID!(IID_ICardAddedEventArgs, 414969752, 61835, 19923, 177, 24, 223, 178, 200, 226, 60, 198);
		RT_INTERFACE!{interface ICardAddedEventArgs(ICardAddedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ICardAddedEventArgs] {
			fn get_SmartCard(&mut self, out: *mut *mut ::rt::gen::windows::devices::smartcards::SmartCard) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ICardRemovedEventArgs, 355670703, 8919, 18757, 175, 201, 3, 180, 111, 66, 166, 205);
		RT_INTERFACE!{interface ICardRemovedEventArgs(ICardRemovedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ICardRemovedEventArgs] {
			fn get_SmartCard(&mut self, out: *mut *mut ::rt::gen::windows::devices::smartcards::SmartCard) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmartCard, 460425329, 25652, 17396, 181, 90, 106, 41, 98, 56, 112, 170);
		RT_INTERFACE!{interface ISmartCard(ISmartCardVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCard] {
			fn get_Reader(&mut self, out: *mut *mut ::rt::gen::windows::devices::smartcards::SmartCardReader) -> ::w::HRESULT,
			fn GetStatusAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::smartcards::SmartCardStatus>) -> ::w::HRESULT,
			fn GetAnswerToResetAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmartCardProvisioningStatics, 327690312, 3347, 20080, 151, 53, 81, 218, 236, 165, 37, 79);
		RT_INTERFACE!{interface ISmartCardProvisioningStatics(ISmartCardProvisioningStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardProvisioningStatics] {
			fn FromSmartCardAsync(&mut self, card: *mut ::rt::gen::windows::devices::smartcards::SmartCard, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::smartcards::SmartCardProvisioning>) -> ::w::HRESULT,
			fn RequestVirtualSmartCardCreationAsync(&mut self, friendlyName: ::w::HSTRING, administrativeKey: *mut ::rt::gen::windows::storage::streams::IBuffer, pinPolicy: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinPolicy, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::smartcards::SmartCardProvisioning>) -> ::w::HRESULT,
			fn RequestVirtualSmartCardCreationAsyncWithCardId(&mut self, friendlyName: ::w::HSTRING, administrativeKey: *mut ::rt::gen::windows::storage::streams::IBuffer, pinPolicy: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinPolicy, cardId: ::w::GUID, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::smartcards::SmartCardProvisioning>) -> ::w::HRESULT,
			fn RequestVirtualSmartCardDeletionAsync(&mut self, card: *mut ::rt::gen::windows::devices::smartcards::SmartCard, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT
		}}
		RT_CLASS!(SmartCardProvisioning: ::rt::gen::windows::devices::smartcards::ISmartCardProvisioning);
		RT_CLASS!(SmartCardPinPolicy: ::rt::gen::windows::devices::smartcards::ISmartCardPinPolicy);
		DEFINE_IID!(IID_ISmartCardProvisioningStatics2, 877119144, 51616, 19414, 181, 13, 37, 31, 78, 141, 58, 98);
		RT_INTERFACE!{interface ISmartCardProvisioningStatics2(ISmartCardProvisioningStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardProvisioningStatics2] {
			fn RequestAttestedVirtualSmartCardCreationAsync(&mut self, friendlyName: ::w::HSTRING, administrativeKey: *mut ::rt::gen::windows::storage::streams::IBuffer, pinPolicy: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinPolicy, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::smartcards::SmartCardProvisioning>) -> ::w::HRESULT,
			fn RequestAttestedVirtualSmartCardCreationAsyncWithCardId(&mut self, friendlyName: ::w::HSTRING, administrativeKey: *mut ::rt::gen::windows::storage::streams::IBuffer, pinPolicy: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinPolicy, cardId: ::w::GUID, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::smartcards::SmartCardProvisioning>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmartCardProvisioning, 435088829, 8107, 18300, 183, 18, 26, 44, 90, 241, 253, 110);
		RT_INTERFACE!{interface ISmartCardProvisioning(ISmartCardProvisioningVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardProvisioning] {
			fn get_SmartCard(&mut self, out: *mut *mut ::rt::gen::windows::devices::smartcards::SmartCard) -> ::w::HRESULT,
			fn GetIdAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::Guid>) -> ::w::HRESULT,
			fn GetNameAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&str>) -> ::w::HRESULT,
			fn GetChallengeContextAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::smartcards::SmartCardChallengeContext>) -> ::w::HRESULT,
			fn RequestPinChangeAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn RequestPinResetAsync(&mut self, handler: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinResetHandler, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT
		}}
		RT_CLASS!(SmartCardChallengeContext: ::rt::gen::windows::devices::smartcards::ISmartCardChallengeContext);
		DEFINE_IID!(IID_SmartCardPinResetHandler, 328031808, 62396, 19036, 180, 29, 75, 78, 246, 132, 226, 55);
		RT_DELEGATE!{delegate SmartCardPinResetHandler(SmartCardPinResetHandlerVtbl, SmartCardPinResetHandlerImpl) [IID_SmartCardPinResetHandler] {
			fn Invoke(&mut self, sender: *mut ::rt::gen::windows::devices::smartcards::SmartCardProvisioning, request: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinResetRequest) -> ::w::HRESULT
		}}
		RT_CLASS!(SmartCardPinResetRequest: ::rt::gen::windows::devices::smartcards::ISmartCardPinResetRequest);
		DEFINE_IID!(IID_ISmartCardProvisioning2, 285026539, 16249, 19302, 155, 124, 17, 193, 73, 183, 208, 188);
		RT_INTERFACE!{interface ISmartCardProvisioning2(ISmartCardProvisioning2Vtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardProvisioning2] {
			fn GetAuthorityKeyContainerNameAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&str>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmartCardPinResetRequest, 318651469, 24505, 20110, 159, 246, 97, 244, 117, 18, 79, 239);
		RT_INTERFACE!{interface ISmartCardPinResetRequest(ISmartCardPinResetRequestVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardPinResetRequest] {
			fn get_Challenge(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_Deadline(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn GetDeferral(&mut self, out: *mut *mut ::rt::gen::windows::devices::smartcards::SmartCardPinResetDeferral) -> ::w::HRESULT,
			fn SetResponse(&mut self, response: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		RT_CLASS!(SmartCardPinResetDeferral: ::rt::gen::windows::devices::smartcards::ISmartCardPinResetDeferral);
		DEFINE_IID!(IID_ISmartCardPinResetDeferral, 415845036, 30725, 16388, 133, 228, 187, 239, 172, 143, 104, 132);
		RT_INTERFACE!{interface ISmartCardPinResetDeferral(ISmartCardPinResetDeferralVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardPinResetDeferral] {
			fn Complete(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmartCardPinPolicy, 406643076, 19894, 18497, 172, 158, 42, 193, 243, 155, 115, 4);
		RT_INTERFACE!{interface ISmartCardPinPolicy(ISmartCardPinPolicyVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardPinPolicy] {
			fn get_MinLength(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_MinLength(&mut self, value: u32) -> ::w::HRESULT,
			fn get_MaxLength(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_MaxLength(&mut self, value: u32) -> ::w::HRESULT,
			fn get_UppercaseLetters(&mut self, out: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinCharacterPolicyOption) -> ::w::HRESULT,
			fn put_UppercaseLetters(&mut self, value: ::rt::gen::windows::devices::smartcards::SmartCardPinCharacterPolicyOption) -> ::w::HRESULT,
			fn get_LowercaseLetters(&mut self, out: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinCharacterPolicyOption) -> ::w::HRESULT,
			fn put_LowercaseLetters(&mut self, value: ::rt::gen::windows::devices::smartcards::SmartCardPinCharacterPolicyOption) -> ::w::HRESULT,
			fn get_Digits(&mut self, out: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinCharacterPolicyOption) -> ::w::HRESULT,
			fn put_Digits(&mut self, value: ::rt::gen::windows::devices::smartcards::SmartCardPinCharacterPolicyOption) -> ::w::HRESULT,
			fn get_SpecialCharacters(&mut self, out: *mut ::rt::gen::windows::devices::smartcards::SmartCardPinCharacterPolicyOption) -> ::w::HRESULT,
			fn put_SpecialCharacters(&mut self, value: ::rt::gen::windows::devices::smartcards::SmartCardPinCharacterPolicyOption) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmartCardConnect, 803178469, 653, 18718, 160, 88, 51, 130, 195, 152, 111, 64);
		RT_INTERFACE!{interface ISmartCardConnect(ISmartCardConnectVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardConnect] {
			fn ConnectAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::smartcards::SmartCardConnection>) -> ::w::HRESULT
		}}
		RT_CLASS!(SmartCardConnection: ::rt::gen::windows::devices::smartcards::ISmartCardConnection);
		DEFINE_IID!(IID_ISmartCardChallengeContext, 422204185, 51652, 18759, 129, 204, 68, 121, 74, 97, 239, 145);
		RT_INTERFACE!{interface ISmartCardChallengeContext(ISmartCardChallengeContextVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardChallengeContext] {
			fn get_Challenge(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn VerifyResponseAsync(&mut self, response: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT,
			fn ProvisionAsync(&mut self, response: *mut ::rt::gen::windows::storage::streams::IBuffer, formatCard: ::w::BOOL, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn ProvisionAsyncWithNewCardId(&mut self, response: *mut ::rt::gen::windows::storage::streams::IBuffer, formatCard: ::w::BOOL, newCardId: ::w::GUID, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn ChangeAdministrativeKeyAsync(&mut self, response: *mut ::rt::gen::windows::storage::streams::IBuffer, newAdministrativeKey: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ISmartCardConnection, 2128320794, 43034, 18364, 166, 73, 21, 107, 230, 183, 242, 49);
		RT_INTERFACE!{interface ISmartCardConnection(ISmartCardConnectionVtbl): IInspectable(IInspectableVtbl) [IID_ISmartCardConnection] {
			fn TransmitAsync(&mut self, command: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT
		}}
} // Windows.Devices.SmartCards
pub mod usb { // Windows.Devices.Usb
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum UsbTransferDirection: i32 {
			Out (UsbTransferDirection_Out) = 0, In (UsbTransferDirection_In) = 1,
		}}
		RT_ENUM! { enum UsbEndpointType: i32 {
			Control (UsbEndpointType_Control) = 0, Isochronous (UsbEndpointType_Isochronous) = 1, Bulk (UsbEndpointType_Bulk) = 2, Interrupt (UsbEndpointType_Interrupt) = 3,
		}}
		RT_ENUM! { enum UsbControlRecipient: i32 {
			Device (UsbControlRecipient_Device) = 0, SpecifiedInterface (UsbControlRecipient_SpecifiedInterface) = 1, Endpoint (UsbControlRecipient_Endpoint) = 2, Other (UsbControlRecipient_Other) = 3, DefaultInterface (UsbControlRecipient_DefaultInterface) = 4,
		}}
		RT_ENUM! { enum UsbControlTransferType: i32 {
			Standard (UsbControlTransferType_Standard) = 0, Class (UsbControlTransferType_Class) = 1, Vendor (UsbControlTransferType_Vendor) = 2,
		}}
		DEFINE_IID!(IID_IUsbControlRequestType, 2392090022, 55101, 18142, 148, 190, 170, 231, 240, 124, 15, 92);
		RT_INTERFACE!{interface IUsbControlRequestType(IUsbControlRequestTypeVtbl): IInspectable(IInspectableVtbl) [IID_IUsbControlRequestType] {
			fn get_Direction(&mut self, out: *mut ::rt::gen::windows::devices::usb::UsbTransferDirection) -> ::w::HRESULT,
			fn put_Direction(&mut self, value: ::rt::gen::windows::devices::usb::UsbTransferDirection) -> ::w::HRESULT,
			fn get_ControlTransferType(&mut self, out: *mut ::rt::gen::windows::devices::usb::UsbControlTransferType) -> ::w::HRESULT,
			fn put_ControlTransferType(&mut self, value: ::rt::gen::windows::devices::usb::UsbControlTransferType) -> ::w::HRESULT,
			fn get_Recipient(&mut self, out: *mut ::rt::gen::windows::devices::usb::UsbControlRecipient) -> ::w::HRESULT,
			fn put_Recipient(&mut self, value: ::rt::gen::windows::devices::usb::UsbControlRecipient) -> ::w::HRESULT,
			fn get_AsByte(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn put_AsByte(&mut self, value: u8) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbControlRequestType: ::rt::gen::windows::devices::usb::IUsbControlRequestType);
		DEFINE_IID!(IID_IUsbSetupPacketFactory, 3374677328, 6958, 19009, 162, 167, 51, 143, 12, 239, 60, 20);
		RT_INTERFACE!{interface IUsbSetupPacketFactory(IUsbSetupPacketFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IUsbSetupPacketFactory] {
			fn CreateWithEightByteBuffer(&mut self, eightByteBuffer: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::devices::usb::UsbSetupPacket) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbSetupPacket: ::rt::gen::windows::devices::usb::IUsbSetupPacket);
		DEFINE_IID!(IID_IUsbSetupPacket, 273391922, 51087, 19537, 182, 84, 228, 157, 2, 242, 203, 3);
		RT_INTERFACE!{interface IUsbSetupPacket(IUsbSetupPacketVtbl): IInspectable(IInspectableVtbl) [IID_IUsbSetupPacket] {
			fn get_RequestType(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbControlRequestType) -> ::w::HRESULT,
			fn put_RequestType(&mut self, value: *mut ::rt::gen::windows::devices::usb::UsbControlRequestType) -> ::w::HRESULT,
			fn get_Request(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn put_Request(&mut self, value: u8) -> ::w::HRESULT,
			fn get_Value(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_Value(&mut self, value: u32) -> ::w::HRESULT,
			fn get_Index(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_Index(&mut self, value: u32) -> ::w::HRESULT,
			fn get_Length(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_Length(&mut self, value: u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbDeviceClass, 85541625, 33886, 18411, 177, 42, 56, 242, 246, 23, 175, 231);
		RT_INTERFACE!{interface IUsbDeviceClass(IUsbDeviceClassVtbl): IInspectable(IInspectableVtbl) [IID_IUsbDeviceClass] {
			fn get_ClassCode(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn put_ClassCode(&mut self, value: u8) -> ::w::HRESULT,
			fn get_SubclassCode(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u8>) -> ::w::HRESULT,
			fn put_SubclassCode(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<u8>) -> ::w::HRESULT,
			fn get_ProtocolCode(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u8>) -> ::w::HRESULT,
			fn put_ProtocolCode(&mut self, value: *mut ::rt::gen::windows::foundation::IReference<u8>) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbDeviceClass: ::rt::gen::windows::devices::usb::IUsbDeviceClass);
		DEFINE_IID!(IID_IUsbDeviceClassesStatics, 2987066663, 50560, 17817, 161, 101, 152, 27, 79, 208, 50, 48);
		RT_INTERFACE!{interface IUsbDeviceClassesStatics(IUsbDeviceClassesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IUsbDeviceClassesStatics] {
			fn get_CdcControl(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT,
			fn get_Physical(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT,
			fn get_PersonalHealthcare(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT,
			fn get_ActiveSync(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT,
			fn get_PalmSync(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT,
			fn get_DeviceFirmwareUpdate(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT,
			fn get_Irda(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT,
			fn get_Measurement(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT,
			fn get_VendorSpecific(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceClass) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbDeviceClasses, 1752143197, 39826, 19248, 151, 129, 194, 44, 85, 172, 53, 203);
		RT_INTERFACE!{interface IUsbDeviceClasses(IUsbDeviceClassesVtbl): IInspectable(IInspectableVtbl) [IID_IUsbDeviceClasses] {
			
		}}
		RT_CLASS!(UsbDeviceClasses: ::rt::gen::windows::devices::usb::IUsbDeviceClasses);
		DEFINE_IID!(IID_IUsbDeviceStatics, 107709858, 2487, 17478, 133, 2, 111, 230, 220, 170, 115, 9);
		RT_INTERFACE!{interface IUsbDeviceStatics(IUsbDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IUsbDeviceStatics] {
			fn GetDeviceSelector(&mut self, vendorId: u32, productId: u32, winUsbInterfaceClass: ::w::GUID, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorGuidOnly(&mut self, winUsbInterfaceClass: ::w::GUID, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorVidPidOnly(&mut self, vendorId: u32, productId: u32, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceClassSelector(&mut self, usbClass: *mut ::rt::gen::windows::devices::usb::UsbDeviceClass, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::usb::UsbDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbDevice: ::rt::gen::windows::devices::usb::IUsbDevice);
		RT_CLASS!(UsbInterface: ::rt::gen::windows::devices::usb::IUsbInterface);
		RT_CLASS!(UsbDeviceDescriptor: ::rt::gen::windows::devices::usb::IUsbDeviceDescriptor);
		RT_CLASS!(UsbConfiguration: ::rt::gen::windows::devices::usb::IUsbConfiguration);
		DEFINE_IID!(IID_IUsbDeviceDescriptor, 524866038, 47767, 17186, 185, 44, 181, 177, 137, 33, 101, 136);
		RT_INTERFACE!{interface IUsbDeviceDescriptor(IUsbDeviceDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbDeviceDescriptor] {
			fn get_BcdUsb(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_MaxPacketSize0(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_VendorId(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ProductId(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_BcdDeviceRevision(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_NumberOfConfigurations(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbConfigurationDescriptor, 4061621650, 46146, 16506, 130, 7, 125, 100, 108, 3, 133, 243);
		RT_INTERFACE!{interface IUsbConfigurationDescriptor(IUsbConfigurationDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbConfigurationDescriptor] {
			fn get_ConfigurationValue(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_MaxPowerMilliamps(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_SelfPowered(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_RemoteWakeup(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbConfigurationDescriptorStatics, 1112337811, 59200, 16545, 146, 189, 218, 18, 14, 160, 73, 20);
		RT_INTERFACE!{interface IUsbConfigurationDescriptorStatics(IUsbConfigurationDescriptorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IUsbConfigurationDescriptorStatics] {
			fn TryParse(&mut self, descriptor: *mut ::rt::gen::windows::devices::usb::UsbDescriptor, parsed: *mut *mut ::rt::gen::windows::devices::usb::UsbConfigurationDescriptor, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn Parse(&mut self, descriptor: *mut ::rt::gen::windows::devices::usb::UsbDescriptor, out: *mut *mut ::rt::gen::windows::devices::usb::UsbConfigurationDescriptor) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbDescriptor: ::rt::gen::windows::devices::usb::IUsbDescriptor);
		RT_CLASS!(UsbConfigurationDescriptor: ::rt::gen::windows::devices::usb::IUsbConfigurationDescriptor);
		DEFINE_IID!(IID_IUsbInterfaceDescriptor, 429289671, 47086, 20368, 140, 213, 148, 162, 226, 87, 89, 138);
		RT_INTERFACE!{interface IUsbInterfaceDescriptor(IUsbInterfaceDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterfaceDescriptor] {
			fn get_ClassCode(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_SubclassCode(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_ProtocolCode(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_AlternateSettingNumber(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_InterfaceNumber(&mut self, out: *mut u8) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbInterfaceDescriptorStatics, 3813318645, 30678, 18614, 176, 190, 22, 198, 66, 35, 22, 254);
		RT_INTERFACE!{interface IUsbInterfaceDescriptorStatics(IUsbInterfaceDescriptorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterfaceDescriptorStatics] {
			fn TryParse(&mut self, descriptor: *mut ::rt::gen::windows::devices::usb::UsbDescriptor, parsed: *mut *mut ::rt::gen::windows::devices::usb::UsbInterfaceDescriptor, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn Parse(&mut self, descriptor: *mut ::rt::gen::windows::devices::usb::UsbDescriptor, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterfaceDescriptor) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbInterfaceDescriptor: ::rt::gen::windows::devices::usb::IUsbInterfaceDescriptor);
		DEFINE_IID!(IID_IUsbEndpointDescriptor, 1799906009, 36343, 19264, 172, 131, 87, 143, 19, 159, 5, 117);
		RT_INTERFACE!{interface IUsbEndpointDescriptor(IUsbEndpointDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbEndpointDescriptor] {
			fn get_EndpointNumber(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Direction(&mut self, out: *mut ::rt::gen::windows::devices::usb::UsbTransferDirection) -> ::w::HRESULT,
			fn get_EndpointType(&mut self, out: *mut ::rt::gen::windows::devices::usb::UsbEndpointType) -> ::w::HRESULT,
			fn get_AsBulkInEndpointDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbBulkInEndpointDescriptor) -> ::w::HRESULT,
			fn get_AsInterruptInEndpointDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterruptInEndpointDescriptor) -> ::w::HRESULT,
			fn get_AsBulkOutEndpointDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbBulkOutEndpointDescriptor) -> ::w::HRESULT,
			fn get_AsInterruptOutEndpointDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterruptOutEndpointDescriptor) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbBulkInEndpointDescriptor: ::rt::gen::windows::devices::usb::IUsbBulkInEndpointDescriptor);
		RT_CLASS!(UsbInterruptInEndpointDescriptor: ::rt::gen::windows::devices::usb::IUsbInterruptInEndpointDescriptor);
		RT_CLASS!(UsbBulkOutEndpointDescriptor: ::rt::gen::windows::devices::usb::IUsbBulkOutEndpointDescriptor);
		RT_CLASS!(UsbInterruptOutEndpointDescriptor: ::rt::gen::windows::devices::usb::IUsbInterruptOutEndpointDescriptor);
		DEFINE_IID!(IID_IUsbEndpointDescriptorStatics, 3364925953, 39530, 18782, 168, 44, 41, 91, 158, 112, 129, 6);
		RT_INTERFACE!{interface IUsbEndpointDescriptorStatics(IUsbEndpointDescriptorStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IUsbEndpointDescriptorStatics] {
			fn TryParse(&mut self, descriptor: *mut ::rt::gen::windows::devices::usb::UsbDescriptor, parsed: *mut *mut ::rt::gen::windows::devices::usb::UsbEndpointDescriptor, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn Parse(&mut self, descriptor: *mut ::rt::gen::windows::devices::usb::UsbDescriptor, out: *mut *mut ::rt::gen::windows::devices::usb::UsbEndpointDescriptor) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbEndpointDescriptor: ::rt::gen::windows::devices::usb::IUsbEndpointDescriptor);
		DEFINE_IID!(IID_IUsbDescriptor, 176812566, 24477, 18548, 137, 4, 218, 154, 211, 245, 82, 143);
		RT_INTERFACE!{interface IUsbDescriptor(IUsbDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbDescriptor] {
			fn get_Length(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_DescriptorType(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn ReadDescriptorBuffer(&mut self, buffer: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbInterruptInEventArgs, 3081781394, 5144, 18742, 130, 9, 41, 156, 245, 96, 85, 131);
		RT_INTERFACE!{interface IUsbInterruptInEventArgs(IUsbInterruptInEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterruptInEventArgs] {
			fn get_InterruptData(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbInterruptInEventArgs: ::rt::gen::windows::devices::usb::IUsbInterruptInEventArgs);
		RT_ENUM! { enum UsbReadOptions: u32 {
			None (UsbReadOptions_None) = 0, AutoClearStall (UsbReadOptions_AutoClearStall) = 1, OverrideAutomaticBufferManagement (UsbReadOptions_OverrideAutomaticBufferManagement) = 2, IgnoreShortPacket (UsbReadOptions_IgnoreShortPacket) = 4, AllowPartialReads (UsbReadOptions_AllowPartialReads) = 8,
		}}
		RT_ENUM! { enum UsbWriteOptions: u32 {
			None (UsbWriteOptions_None) = 0, AutoClearStall (UsbWriteOptions_AutoClearStall) = 1, ShortPacketTerminate (UsbWriteOptions_ShortPacketTerminate) = 2,
		}}
		DEFINE_IID!(IID_IUsbBulkInPipe, 4028443963, 17736, 19792, 179, 38, 216, 44, 218, 190, 18, 32);
		RT_INTERFACE!{interface IUsbBulkInPipe(IUsbBulkInPipeVtbl): IInspectable(IInspectableVtbl) [IID_IUsbBulkInPipe] {
			fn get_MaxTransferSizeBytes(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_EndpointDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbBulkInEndpointDescriptor) -> ::w::HRESULT,
			fn ClearStallAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn put_ReadOptions(&mut self, value: ::rt::gen::windows::devices::usb::UsbReadOptions) -> ::w::HRESULT,
			fn get_ReadOptions(&mut self, out: *mut ::rt::gen::windows::devices::usb::UsbReadOptions) -> ::w::HRESULT,
			fn FlushBuffer(&mut self) -> ::w::HRESULT,
			fn get_InputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IInputStream) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbInterruptInPipe, 4194332950, 34007, 18631, 138, 63, 76, 11, 35, 95, 46, 166);
		RT_INTERFACE!{interface IUsbInterruptInPipe(IUsbInterruptInPipeVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterruptInPipe] {
			fn get_EndpointDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterruptInEndpointDescriptor) -> ::w::HRESULT,
			fn ClearStallAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn add_DataReceived(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::usb::UsbInterruptInPipe, &::rt::gen::windows::devices::usb::UsbInterruptInEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_DataReceived(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbInterruptInPipe: ::rt::gen::windows::devices::usb::IUsbInterruptInPipe);
		DEFINE_IID!(IID_IUsbBulkOutPipe, 2833903214, 277, 17834, 139, 33, 55, 178, 37, 188, 206, 231);
		RT_INTERFACE!{interface IUsbBulkOutPipe(IUsbBulkOutPipeVtbl): IInspectable(IInspectableVtbl) [IID_IUsbBulkOutPipe] {
			fn get_EndpointDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbBulkOutEndpointDescriptor) -> ::w::HRESULT,
			fn ClearStallAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn put_WriteOptions(&mut self, value: ::rt::gen::windows::devices::usb::UsbWriteOptions) -> ::w::HRESULT,
			fn get_WriteOptions(&mut self, out: *mut ::rt::gen::windows::devices::usb::UsbWriteOptions) -> ::w::HRESULT,
			fn get_OutputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IOutputStream) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbInterruptOutPipe, 3917793449, 43769, 18896, 185, 108, 246, 97, 171, 74, 127, 149);
		RT_INTERFACE!{interface IUsbInterruptOutPipe(IUsbInterruptOutPipeVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterruptOutPipe] {
			fn get_EndpointDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterruptOutEndpointDescriptor) -> ::w::HRESULT,
			fn ClearStallAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn put_WriteOptions(&mut self, value: ::rt::gen::windows::devices::usb::UsbWriteOptions) -> ::w::HRESULT,
			fn get_WriteOptions(&mut self, out: *mut ::rt::gen::windows::devices::usb::UsbWriteOptions) -> ::w::HRESULT,
			fn get_OutputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IOutputStream) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbBulkInPipe: ::rt::gen::windows::devices::usb::IUsbBulkInPipe);
		RT_CLASS!(UsbBulkOutPipe: ::rt::gen::windows::devices::usb::IUsbBulkOutPipe);
		RT_CLASS!(UsbInterruptOutPipe: ::rt::gen::windows::devices::usb::IUsbInterruptOutPipe);
		DEFINE_IID!(IID_IUsbConfiguration, 1746367529, 13993, 18135, 184, 115, 252, 104, 146, 81, 236, 48);
		RT_INTERFACE!{interface IUsbConfiguration(IUsbConfigurationVtbl): IInspectable(IInspectableVtbl) [IID_IUsbConfiguration] {
			fn get_UsbInterfaces(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbInterface>) -> ::w::HRESULT,
			fn get_ConfigurationDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbConfigurationDescriptor) -> ::w::HRESULT,
			fn get_Descriptors(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbDescriptor>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbInterface, 2687642517, 32583, 18603, 167, 39, 103, 140, 37, 190, 33, 18);
		RT_INTERFACE!{interface IUsbInterface(IUsbInterfaceVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterface] {
			fn get_BulkInPipes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbBulkInPipe>) -> ::w::HRESULT,
			fn get_InterruptInPipes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbInterruptInPipe>) -> ::w::HRESULT,
			fn get_BulkOutPipes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbBulkOutPipe>) -> ::w::HRESULT,
			fn get_InterruptOutPipes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbInterruptOutPipe>) -> ::w::HRESULT,
			fn get_InterfaceSettings(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbInterfaceSetting>) -> ::w::HRESULT,
			fn get_InterfaceNumber(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Descriptors(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbDescriptor>) -> ::w::HRESULT
		}}
		RT_CLASS!(UsbInterfaceSetting: ::rt::gen::windows::devices::usb::IUsbInterfaceSetting);
		DEFINE_IID!(IID_IUsbInterfaceSetting, 405257127, 36263, 19191, 143, 76, 127, 48, 50, 231, 129, 245);
		RT_INTERFACE!{interface IUsbInterfaceSetting(IUsbInterfaceSettingVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterfaceSetting] {
			fn get_BulkInEndpoints(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbBulkInEndpointDescriptor>) -> ::w::HRESULT,
			fn get_InterruptInEndpoints(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbInterruptInEndpointDescriptor>) -> ::w::HRESULT,
			fn get_BulkOutEndpoints(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbBulkOutEndpointDescriptor>) -> ::w::HRESULT,
			fn get_InterruptOutEndpoints(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbInterruptOutEndpointDescriptor>) -> ::w::HRESULT,
			fn get_Selected(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn SelectSettingAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn get_InterfaceDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterfaceDescriptor) -> ::w::HRESULT,
			fn get_Descriptors(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::usb::UsbDescriptor>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbBulkInEndpointDescriptor, 1013860422, 1743, 17065, 157, 194, 151, 28, 27, 20, 182, 227);
		RT_INTERFACE!{interface IUsbBulkInEndpointDescriptor(IUsbBulkInEndpointDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbBulkInEndpointDescriptor] {
			fn get_MaxPacketSize(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_EndpointNumber(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Pipe(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbBulkInPipe) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbInterruptInEndpointDescriptor, 3226634599, 51473, 19514, 134, 178, 65, 156, 45, 168, 144, 57);
		RT_INTERFACE!{interface IUsbInterruptInEndpointDescriptor(IUsbInterruptInEndpointDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterruptInEndpointDescriptor] {
			fn get_MaxPacketSize(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_EndpointNumber(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Interval(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_Pipe(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterruptInPipe) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbBulkOutEndpointDescriptor, 673219706, 65518, 20320, 155, 225, 149, 108, 172, 62, 203, 101);
		RT_INTERFACE!{interface IUsbBulkOutEndpointDescriptor(IUsbBulkOutEndpointDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbBulkOutEndpointDescriptor] {
			fn get_MaxPacketSize(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_EndpointNumber(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Pipe(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbBulkOutPipe) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbInterruptOutEndpointDescriptor, 3433033089, 4298, 17715, 149, 45, 158, 39, 131, 65, 232, 15);
		RT_INTERFACE!{interface IUsbInterruptOutEndpointDescriptor(IUsbInterruptOutEndpointDescriptorVtbl): IInspectable(IInspectableVtbl) [IID_IUsbInterruptOutEndpointDescriptor] {
			fn get_MaxPacketSize(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_EndpointNumber(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_Interval(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_Pipe(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterruptOutPipe) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUsbDevice, 1380563346, 50262, 17621, 173, 94, 36, 245, 160, 137, 246, 59);
		RT_INTERFACE!{interface IUsbDevice(IUsbDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IUsbDevice] {
			fn SendControlOutTransferAsync(&mut self, setupPacket: *mut ::rt::gen::windows::devices::usb::UsbSetupPacket, buffer: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<u32>) -> ::w::HRESULT,
			fn SendControlOutTransferAsyncNoBuffer(&mut self, setupPacket: *mut ::rt::gen::windows::devices::usb::UsbSetupPacket, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<u32>) -> ::w::HRESULT,
			fn SendControlInTransferAsync(&mut self, setupPacket: *mut ::rt::gen::windows::devices::usb::UsbSetupPacket, buffer: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT,
			fn SendControlInTransferAsyncNoBuffer(&mut self, setupPacket: *mut ::rt::gen::windows::devices::usb::UsbSetupPacket, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IBuffer>) -> ::w::HRESULT,
			fn get_DefaultInterface(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbInterface) -> ::w::HRESULT,
			fn get_DeviceDescriptor(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbDeviceDescriptor) -> ::w::HRESULT,
			fn get_Configuration(&mut self, out: *mut *mut ::rt::gen::windows::devices::usb::UsbConfiguration) -> ::w::HRESULT
		}}
} // Windows.Devices.Usb
pub mod wifi { // Windows.Devices.WiFi
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum WiFiNetworkKind: i32 {
			Any (WiFiNetworkKind_Any) = 0, Infrastructure (WiFiNetworkKind_Infrastructure) = 1, Adhoc (WiFiNetworkKind_Adhoc) = 2,
		}}
		RT_ENUM! { enum WiFiPhyKind: i32 {
			Unknown (WiFiPhyKind_Unknown) = 0, Fhss (WiFiPhyKind_Fhss) = 1, Dsss (WiFiPhyKind_Dsss) = 2, IRBaseband (WiFiPhyKind_IRBaseband) = 3, Ofdm (WiFiPhyKind_Ofdm) = 4, Hrdsss (WiFiPhyKind_Hrdsss) = 5, Erp (WiFiPhyKind_Erp) = 6, HT (WiFiPhyKind_HT) = 7, Vht (WiFiPhyKind_Vht) = 8,
		}}
		RT_ENUM! { enum WiFiAccessStatus: i32 {
			Unspecified (WiFiAccessStatus_Unspecified) = 0, Allowed (WiFiAccessStatus_Allowed) = 1, DeniedByUser (WiFiAccessStatus_DeniedByUser) = 2, DeniedBySystem (WiFiAccessStatus_DeniedBySystem) = 3,
		}}
		RT_ENUM! { enum WiFiReconnectionKind: i32 {
			Automatic (WiFiReconnectionKind_Automatic) = 0, Manual (WiFiReconnectionKind_Manual) = 1,
		}}
		RT_ENUM! { enum WiFiConnectionStatus: i32 {
			UnspecifiedFailure (WiFiConnectionStatus_UnspecifiedFailure) = 0, Success (WiFiConnectionStatus_Success) = 1, AccessRevoked (WiFiConnectionStatus_AccessRevoked) = 2, InvalidCredential (WiFiConnectionStatus_InvalidCredential) = 3, NetworkNotAvailable (WiFiConnectionStatus_NetworkNotAvailable) = 4, Timeout (WiFiConnectionStatus_Timeout) = 5, UnsupportedAuthenticationProtocol (WiFiConnectionStatus_UnsupportedAuthenticationProtocol) = 6,
		}}
		DEFINE_IID!(IID_IWiFiAdapterStatics, 3659922909, 53836, 17379, 170, 189, 196, 101, 159, 115, 15, 153);
		RT_INTERFACE!{interface IWiFiAdapterStatics(IWiFiAdapterStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiAdapterStatics] {
			fn FindAllAdaptersAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::wifi::WiFiAdapter>>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifi::WiFiAdapter>) -> ::w::HRESULT,
			fn RequestAccessAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<::rt::gen::windows::devices::wifi::WiFiAccessStatus>) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiAdapter: ::rt::gen::windows::devices::wifi::IWiFiAdapter);
		DEFINE_IID!(IID_IWiFiAdapter, 2797921315, 15733, 17316, 185, 222, 17, 226, 107, 114, 217, 176);
		RT_INTERFACE!{interface IWiFiAdapter(IWiFiAdapterVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiAdapter] {
			fn get_NetworkAdapter(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::NetworkAdapter) -> ::w::HRESULT,
			fn ScanAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn get_NetworkReport(&mut self, out: *mut *mut ::rt::gen::windows::devices::wifi::WiFiNetworkReport) -> ::w::HRESULT,
			fn add_AvailableNetworksChanged(&mut self, args: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifi::WiFiAdapter, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AvailableNetworksChanged(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn ConnectAsync(&mut self, availableNetwork: *mut ::rt::gen::windows::devices::wifi::WiFiAvailableNetwork, reconnectionKind: ::rt::gen::windows::devices::wifi::WiFiReconnectionKind, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifi::WiFiConnectionResult>) -> ::w::HRESULT,
			fn ConnectWithPasswordCredentialAsync(&mut self, availableNetwork: *mut ::rt::gen::windows::devices::wifi::WiFiAvailableNetwork, reconnectionKind: ::rt::gen::windows::devices::wifi::WiFiReconnectionKind, passwordCredential: *mut ::rt::gen::windows::security::credentials::PasswordCredential, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifi::WiFiConnectionResult>) -> ::w::HRESULT,
			fn ConnectWithPasswordCredentialAndSsidAsync(&mut self, availableNetwork: *mut ::rt::gen::windows::devices::wifi::WiFiAvailableNetwork, reconnectionKind: ::rt::gen::windows::devices::wifi::WiFiReconnectionKind, passwordCredential: *mut ::rt::gen::windows::security::credentials::PasswordCredential, ssid: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifi::WiFiConnectionResult>) -> ::w::HRESULT,
			fn Disconnect(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiNetworkReport: ::rt::gen::windows::devices::wifi::IWiFiNetworkReport);
		RT_CLASS!(WiFiAvailableNetwork: ::rt::gen::windows::devices::wifi::IWiFiAvailableNetwork);
		RT_CLASS!(WiFiConnectionResult: ::rt::gen::windows::devices::wifi::IWiFiConnectionResult);
		DEFINE_IID!(IID_IWiFiNetworkReport, 2502221522, 22801, 17502, 129, 148, 190, 79, 26, 112, 72, 149);
		RT_INTERFACE!{interface IWiFiNetworkReport(IWiFiNetworkReportVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiNetworkReport] {
			fn get_Timestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_AvailableNetworks(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::devices::wifi::WiFiAvailableNetwork>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiAvailableNetwork, 652829254, 6206, 18180, 152, 38, 113, 180, 162, 240, 246, 104);
		RT_INTERFACE!{interface IWiFiAvailableNetwork(IWiFiAvailableNetworkVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiAvailableNetwork] {
			fn get_Uptime(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_Ssid(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Bssid(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ChannelCenterFrequencyInKilohertz(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_NetworkRssiInDecibelMilliwatts(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_SignalBars(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn get_NetworkKind(&mut self, out: *mut ::rt::gen::windows::devices::wifi::WiFiNetworkKind) -> ::w::HRESULT,
			fn get_PhyKind(&mut self, out: *mut ::rt::gen::windows::devices::wifi::WiFiPhyKind) -> ::w::HRESULT,
			fn get_SecuritySettings(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::NetworkSecuritySettings) -> ::w::HRESULT,
			fn get_BeaconInterval(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn get_IsWiFiDirect(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiConnectionResult, 339468249, 50045, 16574, 165, 200, 133, 123, 206, 133, 169, 49);
		RT_INTERFACE!{interface IWiFiConnectionResult(IWiFiConnectionResultVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiConnectionResult] {
			fn get_ConnectionStatus(&mut self, out: *mut ::rt::gen::windows::devices::wifi::WiFiConnectionStatus) -> ::w::HRESULT
		}}
} // Windows.Devices.WiFi
pub mod wifidirect { // Windows.Devices.WiFiDirect
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum WiFiDirectConnectionStatus: i32 {
			Disconnected (WiFiDirectConnectionStatus_Disconnected) = 0, Connected (WiFiDirectConnectionStatus_Connected) = 1,
		}}
		RT_ENUM! { enum WiFiDirectError: i32 {
			Success (WiFiDirectError_Success) = 0, RadioNotAvailable (WiFiDirectError_RadioNotAvailable) = 1, ResourceInUse (WiFiDirectError_ResourceInUse) = 2,
		}}
		RT_ENUM! { enum WiFiDirectDeviceSelectorType: i32 {
			DeviceInterface (WiFiDirectDeviceSelectorType_DeviceInterface) = 0, AssociationEndpoint (WiFiDirectDeviceSelectorType_AssociationEndpoint) = 1,
		}}
		RT_ENUM! { enum WiFiDirectAdvertisementListenStateDiscoverability: i32 {
			None (WiFiDirectAdvertisementListenStateDiscoverability_None) = 0, Normal (WiFiDirectAdvertisementListenStateDiscoverability_Normal) = 1, Intensive (WiFiDirectAdvertisementListenStateDiscoverability_Intensive) = 2,
		}}
		RT_ENUM! { enum WiFiDirectAdvertisementPublisherStatus: i32 {
			Created (WiFiDirectAdvertisementPublisherStatus_Created) = 0, Started (WiFiDirectAdvertisementPublisherStatus_Started) = 1, Stopped (WiFiDirectAdvertisementPublisherStatus_Stopped) = 2, Aborted (WiFiDirectAdvertisementPublisherStatus_Aborted) = 3,
		}}
		RT_ENUM! { enum WiFiDirectConfigurationMethod: i32 {
			ProvidePin (WiFiDirectConfigurationMethod_ProvidePin) = 0, DisplayPin (WiFiDirectConfigurationMethod_DisplayPin) = 1, PushButton (WiFiDirectConfigurationMethod_PushButton) = 2,
		}}
		RT_ENUM! { enum WiFiDirectPairingProcedure: i32 {
			GroupOwnerNegotiation (WiFiDirectPairingProcedure_GroupOwnerNegotiation) = 0, Invitation (WiFiDirectPairingProcedure_Invitation) = 1,
		}}
		DEFINE_IID!(IID_IWiFiDirectDeviceStatics, 3899438460, 15020, 18513, 167, 146, 72, 42, 175, 147, 27, 4);
		RT_INTERFACE!{interface IWiFiDirectDeviceStatics(IWiFiDirectDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectDeviceStatics] {
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifidirect::WiFiDirectDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectDevice: ::rt::gen::windows::devices::wifidirect::IWiFiDirectDevice);
		DEFINE_IID!(IID_IWiFiDirectDeviceStatics2, 445988425, 45315, 17278, 146, 38, 171, 103, 151, 19, 66, 249);
		RT_INTERFACE!{interface IWiFiDirectDeviceStatics2(IWiFiDirectDeviceStatics2Vtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectDeviceStatics2] {
			fn GetDeviceSelector(&mut self, type_: ::rt::gen::windows::devices::wifidirect::WiFiDirectDeviceSelectorType, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, connectionParameters: *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectConnectionParameters, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifidirect::WiFiDirectDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectConnectionParameters: ::rt::gen::windows::devices::wifidirect::IWiFiDirectConnectionParameters);
		DEFINE_IID!(IID_IWiFiDirectInformationElementStatics, 3687853846, 4517, 20064, 140, 170, 52, 119, 33, 72, 55, 138);
		RT_INTERFACE!{interface IWiFiDirectInformationElementStatics(IWiFiDirectInformationElementStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectInformationElementStatics] {
			fn CreateFromBuffer(&mut self, buffer: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::wifidirect::WiFiDirectInformationElement>) -> ::w::HRESULT,
			fn CreateFromDeviceInformation(&mut self, deviceInformation: *mut ::rt::gen::windows::devices::enumeration::DeviceInformation, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::wifidirect::WiFiDirectInformationElement>) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectInformationElement: ::rt::gen::windows::devices::wifidirect::IWiFiDirectInformationElement);
		DEFINE_IID!(IID_IWiFiDirectInformationElement, 2952491734, 30395, 18814, 172, 139, 220, 114, 131, 139, 195, 9);
		RT_INTERFACE!{interface IWiFiDirectInformationElement(IWiFiDirectInformationElementVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectInformationElement] {
			fn get_Oui(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_Oui(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_OuiType(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn put_OuiType(&mut self, value: u8) -> ::w::HRESULT,
			fn get_Value(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_Value(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiDirectLegacySettings, 2790251450, 62205, 17767, 169, 27, 245, 194, 245, 50, 16, 87);
		RT_INTERFACE!{interface IWiFiDirectLegacySettings(IWiFiDirectLegacySettingsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectLegacySettings] {
			fn get_IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_Ssid(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_Ssid(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_Passphrase(&mut self, out: *mut *mut ::rt::gen::windows::security::credentials::PasswordCredential) -> ::w::HRESULT,
			fn put_Passphrase(&mut self, value: *mut ::rt::gen::windows::security::credentials::PasswordCredential) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectLegacySettings: ::rt::gen::windows::devices::wifidirect::IWiFiDirectLegacySettings);
		DEFINE_IID!(IID_IWiFiDirectAdvertisement, 2874219053, 10758, 18849, 165, 132, 97, 67, 92, 121, 5, 166);
		RT_INTERFACE!{interface IWiFiDirectAdvertisement(IWiFiDirectAdvertisementVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectAdvertisement] {
			fn get_InformationElements(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::wifidirect::WiFiDirectInformationElement>) -> ::w::HRESULT,
			fn put_InformationElements(&mut self, value: *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::devices::wifidirect::WiFiDirectInformationElement>) -> ::w::HRESULT,
			fn get_ListenStateDiscoverability(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisementListenStateDiscoverability) -> ::w::HRESULT,
			fn put_ListenStateDiscoverability(&mut self, value: ::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisementListenStateDiscoverability) -> ::w::HRESULT,
			fn get_IsAutonomousGroupOwnerEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_IsAutonomousGroupOwnerEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_LegacySettings(&mut self, out: *mut *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectLegacySettings) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiDirectAdvertisement2, 3076106822, 55318, 18715, 145, 122, 180, 13, 125, 196, 3, 162);
		RT_INTERFACE!{interface IWiFiDirectAdvertisement2(IWiFiDirectAdvertisement2Vtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectAdvertisement2] {
			fn get_SupportedConfigurationMethods(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<::rt::gen::windows::devices::wifidirect::WiFiDirectConfigurationMethod>) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectAdvertisement: ::rt::gen::windows::devices::wifidirect::IWiFiDirectAdvertisement);
		DEFINE_IID!(IID_IWiFiDirectAdvertisementPublisherStatusChangedEventArgs, 2868766012, 21633, 18150, 144, 221, 50, 17, 101, 24, 241, 146);
		RT_INTERFACE!{interface IWiFiDirectAdvertisementPublisherStatusChangedEventArgs(IWiFiDirectAdvertisementPublisherStatusChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectAdvertisementPublisherStatusChangedEventArgs] {
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisementPublisherStatus) -> ::w::HRESULT,
			fn get_Error(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectError) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectAdvertisementPublisherStatusChangedEventArgs: ::rt::gen::windows::devices::wifidirect::IWiFiDirectAdvertisementPublisherStatusChangedEventArgs);
		DEFINE_IID!(IID_IWiFiDirectAdvertisementPublisher, 3009031450, 39711, 17881, 146, 90, 105, 77, 102, 223, 104, 239);
		RT_INTERFACE!{interface IWiFiDirectAdvertisementPublisher(IWiFiDirectAdvertisementPublisherVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectAdvertisementPublisher] {
			fn get_Advertisement(&mut self, out: *mut *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisement) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisementPublisherStatus) -> ::w::HRESULT,
			fn add_StatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisementPublisher, &::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisementPublisherStatusChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_StatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectAdvertisementPublisher: ::rt::gen::windows::devices::wifidirect::IWiFiDirectAdvertisementPublisher);
		DEFINE_IID!(IID_IWiFiDirectConnectionParametersStatics, 1502278803, 30274, 17775, 185, 216, 232, 169, 235, 31, 64, 26);
		RT_INTERFACE!{interface IWiFiDirectConnectionParametersStatics(IWiFiDirectConnectionParametersStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectConnectionParametersStatics] {
			fn GetDevicePairingKinds(&mut self, configurationMethod: ::rt::gen::windows::devices::wifidirect::WiFiDirectConfigurationMethod, out: *mut ::rt::gen::windows::devices::enumeration::DevicePairingKinds) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiDirectConnectionParameters, 3001373701, 22274, 19222, 160, 44, 187, 205, 33, 239, 96, 152);
		RT_INTERFACE!{interface IWiFiDirectConnectionParameters(IWiFiDirectConnectionParametersVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectConnectionParameters] {
			fn get_GroupOwnerIntent(&mut self, out: *mut i16) -> ::w::HRESULT,
			fn put_GroupOwnerIntent(&mut self, value: i16) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiDirectConnectionParameters2, 2872774590, 43650, 17588, 136, 200, 227, 5, 107, 137, 128, 29);
		RT_INTERFACE!{interface IWiFiDirectConnectionParameters2(IWiFiDirectConnectionParameters2Vtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectConnectionParameters2] {
			fn get_PreferenceOrderedConfigurationMethods(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<::rt::gen::windows::devices::wifidirect::WiFiDirectConfigurationMethod>) -> ::w::HRESULT,
			fn get_PreferredPairingProcedure(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectPairingProcedure) -> ::w::HRESULT,
			fn put_PreferredPairingProcedure(&mut self, value: ::rt::gen::windows::devices::wifidirect::WiFiDirectPairingProcedure) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectConnectionRequest: ::rt::gen::windows::devices::wifidirect::IWiFiDirectConnectionRequest);
		DEFINE_IID!(IID_IWiFiDirectConnectionRequestedEventArgs, 4187824318, 54157, 18511, 130, 21, 231, 182, 90, 191, 36, 76);
		RT_INTERFACE!{interface IWiFiDirectConnectionRequestedEventArgs(IWiFiDirectConnectionRequestedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectConnectionRequestedEventArgs] {
			fn GetConnectionRequest(&mut self, out: *mut *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectConnectionRequest) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectConnectionRequestedEventArgs: ::rt::gen::windows::devices::wifidirect::IWiFiDirectConnectionRequestedEventArgs);
		DEFINE_IID!(IID_IWiFiDirectConnectionListener, 1771838221, 36115, 20201, 185, 236, 156, 114, 248, 37, 31, 125);
		RT_INTERFACE!{interface IWiFiDirectConnectionListener(IWiFiDirectConnectionListenerVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectConnectionListener] {
			fn add_ConnectionRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::WiFiDirectConnectionListener, &::rt::gen::windows::devices::wifidirect::WiFiDirectConnectionRequestedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ConnectionRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectConnectionListener: ::rt::gen::windows::devices::wifidirect::IWiFiDirectConnectionListener);
		DEFINE_IID!(IID_IWiFiDirectDevice, 1927195304, 29419, 19886, 138, 40, 133, 19, 53, 93, 39, 119);
		RT_INTERFACE!{interface IWiFiDirectDevice(IWiFiDirectDeviceVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectDevice] {
			fn get_ConnectionStatus(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::WiFiDirectConnectionStatus) -> ::w::HRESULT,
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn add_ConnectionStatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::WiFiDirectDevice, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ConnectionStatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn GetConnectionEndpointPairs(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::networking::EndpointPair>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiDirectConnectionRequest, 2394527237, 37199, 18883, 166, 20, 209, 141, 197, 177, 155, 67);
		RT_INTERFACE!{interface IWiFiDirectConnectionRequest(IWiFiDirectConnectionRequestVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectConnectionRequest] {
			fn get_DeviceInformation(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT
		}}
pub mod services { // Windows.Devices.WiFiDirect.Services
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum WiFiDirectServiceConfigurationMethod: i32 {
			Default (WiFiDirectServiceConfigurationMethod_Default) = 0, PinDisplay (WiFiDirectServiceConfigurationMethod_PinDisplay) = 1, PinEntry (WiFiDirectServiceConfigurationMethod_PinEntry) = 2,
		}}
		RT_ENUM! { enum WiFiDirectServiceStatus: i32 {
			Available (WiFiDirectServiceStatus_Available) = 0, Busy (WiFiDirectServiceStatus_Busy) = 1, Custom (WiFiDirectServiceStatus_Custom) = 2,
		}}
		RT_ENUM! { enum WiFiDirectServiceSessionStatus: i32 {
			Closed (WiFiDirectServiceSessionStatus_Closed) = 0, Initiated (WiFiDirectServiceSessionStatus_Initiated) = 1, Requested (WiFiDirectServiceSessionStatus_Requested) = 2, Open (WiFiDirectServiceSessionStatus_Open) = 3,
		}}
		RT_ENUM! { enum WiFiDirectServiceSessionErrorStatus: i32 {
			Ok (WiFiDirectServiceSessionErrorStatus_Ok) = 0, Disassociated (WiFiDirectServiceSessionErrorStatus_Disassociated) = 1, LocalClose (WiFiDirectServiceSessionErrorStatus_LocalClose) = 2, RemoteClose (WiFiDirectServiceSessionErrorStatus_RemoteClose) = 3, SystemFailure (WiFiDirectServiceSessionErrorStatus_SystemFailure) = 4, NoResponseFromRemote (WiFiDirectServiceSessionErrorStatus_NoResponseFromRemote) = 5,
		}}
		RT_ENUM! { enum WiFiDirectServiceAdvertisementStatus: i32 {
			Created (WiFiDirectServiceAdvertisementStatus_Created) = 0, Started (WiFiDirectServiceAdvertisementStatus_Started) = 1, Stopped (WiFiDirectServiceAdvertisementStatus_Stopped) = 2, Aborted (WiFiDirectServiceAdvertisementStatus_Aborted) = 3,
		}}
		RT_ENUM! { enum WiFiDirectServiceError: i32 {
			Success (WiFiDirectServiceError_Success) = 0, RadioNotAvailable (WiFiDirectServiceError_RadioNotAvailable) = 1, ResourceInUse (WiFiDirectServiceError_ResourceInUse) = 2, UnsupportedHardware (WiFiDirectServiceError_UnsupportedHardware) = 3, NoHardware (WiFiDirectServiceError_NoHardware) = 4,
		}}
		RT_ENUM! { enum WiFiDirectServiceIPProtocol: i32 {
			Tcp (WiFiDirectServiceIPProtocol_Tcp) = 6, Udp (WiFiDirectServiceIPProtocol_Udp) = 17,
		}}
		DEFINE_IID!(IID_IWiFiDirectServiceProvisioningInfo, 2346417406, 38873, 17826, 142, 153, 219, 80, 145, 15, 182, 166);
		RT_INTERFACE!{interface IWiFiDirectServiceProvisioningInfo(IWiFiDirectServiceProvisioningInfoVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceProvisioningInfo] {
			fn get_SelectedConfigurationMethod(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceConfigurationMethod) -> ::w::HRESULT,
			fn get_IsGroupFormationNeeded(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectServiceProvisioningInfo: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectServiceProvisioningInfo);
		DEFINE_IID!(IID_IWiFiDirectServiceAutoAcceptSessionConnectedEventArgs, 3705266206, 33759, 17381, 143, 67, 203, 232, 71, 158, 132, 235);
		RT_INTERFACE!{interface IWiFiDirectServiceAutoAcceptSessionConnectedEventArgs(IWiFiDirectServiceAutoAcceptSessionConnectedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceAutoAcceptSessionConnectedEventArgs] {
			fn get_Session(&mut self, out: *mut *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession) -> ::w::HRESULT,
			fn get_SessionInfo(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectServiceSession: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectServiceSession);
		RT_CLASS!(WiFiDirectServiceAutoAcceptSessionConnectedEventArgs: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectServiceAutoAcceptSessionConnectedEventArgs);
		DEFINE_IID!(IID_IWiFiDirectServiceRemotePortAddedEventArgs, 3570318017, 16339, 20238, 183, 189, 120, 41, 6, 244, 68, 17);
		RT_INTERFACE!{interface IWiFiDirectServiceRemotePortAddedEventArgs(IWiFiDirectServiceRemotePortAddedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceRemotePortAddedEventArgs] {
			fn get_EndpointPairs(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::networking::EndpointPair>) -> ::w::HRESULT,
			fn get_Protocol(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceIPProtocol) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectServiceRemotePortAddedEventArgs: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectServiceRemotePortAddedEventArgs);
		DEFINE_IID!(IID_IWiFiDirectServiceSessionDeferredEventArgs, 2382109055, 4609, 20255, 182, 244, 93, 241, 183, 185, 251, 46);
		RT_INTERFACE!{interface IWiFiDirectServiceSessionDeferredEventArgs(IWiFiDirectServiceSessionDeferredEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceSessionDeferredEventArgs] {
			fn get_DeferredSessionInfo(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectServiceSessionDeferredEventArgs: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectServiceSessionDeferredEventArgs);
		DEFINE_IID!(IID_IWiFiDirectServiceSessionRequestedEventArgs, 1958595601, 21462, 18841, 180, 248, 108, 142, 204, 23, 113, 231);
		RT_INTERFACE!{interface IWiFiDirectServiceSessionRequestedEventArgs(IWiFiDirectServiceSessionRequestedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceSessionRequestedEventArgs] {
			fn GetSessionRequest(&mut self, out: *mut *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSessionRequest) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectServiceSessionRequest: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectServiceSessionRequest);
		RT_CLASS!(WiFiDirectServiceSessionRequestedEventArgs: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectServiceSessionRequestedEventArgs);
		DEFINE_IID!(IID_IWiFiDirectServiceAdvertiserFactory, 822520845, 46150, 20243, 159, 154, 138, 233, 37, 254, 186, 43);
		RT_INTERFACE!{interface IWiFiDirectServiceAdvertiserFactory(IWiFiDirectServiceAdvertiserFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceAdvertiserFactory] {
			fn CreateWiFiDirectServiceAdvertiser(&mut self, serviceName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAdvertiser) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectServiceAdvertiser: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectServiceAdvertiser);
		DEFINE_IID!(IID_IWiFiDirectServiceAdvertiser, 2762612449, 40335, 20303, 147, 238, 125, 222, 162, 227, 127, 70);
		RT_INTERFACE!{interface IWiFiDirectServiceAdvertiser(IWiFiDirectServiceAdvertiserVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceAdvertiser] {
			fn get_ServiceName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ServiceNamePrefixes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn get_ServiceInfo(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_ServiceInfo(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_AutoAcceptSession(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_AutoAcceptSession(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_PreferGroupOwnerMode(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_PreferGroupOwnerMode(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_PreferredConfigurationMethods(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceConfigurationMethod>) -> ::w::HRESULT,
			fn get_ServiceStatus(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceStatus) -> ::w::HRESULT,
			fn put_ServiceStatus(&mut self, value: ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceStatus) -> ::w::HRESULT,
			fn get_CustomServiceStatusCode(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_CustomServiceStatusCode(&mut self, value: u32) -> ::w::HRESULT,
			fn get_DeferredSessionInfo(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_DeferredSessionInfo(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_AdvertisementStatus(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAdvertisementStatus) -> ::w::HRESULT,
			fn get_ServiceError(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceError) -> ::w::HRESULT,
			fn add_SessionRequested(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAdvertiser, &::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSessionRequestedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SessionRequested(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_AutoAcceptSessionConnected(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAdvertiser, &::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAutoAcceptSessionConnectedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AutoAcceptSessionConnected(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn add_AdvertisementStatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAdvertiser, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_AdvertisementStatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn ConnectAsync(&mut self, deviceInfo: *mut ::rt::gen::windows::devices::enumeration::DeviceInformation, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession>) -> ::w::HRESULT,
			fn ConnectAsyncWithPin(&mut self, deviceInfo: *mut ::rt::gen::windows::devices::enumeration::DeviceInformation, pin: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession>) -> ::w::HRESULT,
			fn Start(&mut self) -> ::w::HRESULT,
			fn Stop(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiDirectServiceStatics, 2108948549, 64884, 18056, 183, 37, 93, 206, 134, 172, 242, 51);
		RT_INTERFACE!{interface IWiFiDirectServiceStatics(IWiFiDirectServiceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceStatics] {
			fn GetSelector(&mut self, serviceName: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetSelectorWithFilter(&mut self, serviceName: ::w::HSTRING, serviceInfoFilter: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectService>) -> ::w::HRESULT
		}}
		RT_CLASS!(WiFiDirectService: ::rt::gen::windows::devices::wifidirect::services::IWiFiDirectService);
		DEFINE_IID!(IID_IWiFiDirectService, 1353366456, 24433, 17900, 132, 241, 161, 228, 252, 120, 121, 163);
		RT_INTERFACE!{interface IWiFiDirectService(IWiFiDirectServiceVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectService] {
			fn get_RemoteServiceInfo(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_SupportedConfigurationMethods(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceConfigurationMethod>) -> ::w::HRESULT,
			fn get_PreferGroupOwnerMode(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_PreferGroupOwnerMode(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_SessionInfo(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn put_SessionInfo(&mut self, value: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_ServiceError(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceError) -> ::w::HRESULT,
			fn add_SessionDeferred(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectService, &::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSessionDeferredEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SessionDeferred(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn GetProvisioningInfoAsync(&mut self, selectedConfigurationMethod: ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceConfigurationMethod, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceProvisioningInfo>) -> ::w::HRESULT,
			fn ConnectAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession>) -> ::w::HRESULT,
			fn ConnectAsyncWithPin(&mut self, pin: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiDirectServiceSessionRequest, 2699197579, 20683, 19032, 155, 207, 228, 114, 185, 159, 186, 4);
		RT_INTERFACE!{interface IWiFiDirectServiceSessionRequest(IWiFiDirectServiceSessionRequestVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceSessionRequest] {
			fn get_DeviceInformation(&mut self, out: *mut *mut ::rt::gen::windows::devices::enumeration::DeviceInformation) -> ::w::HRESULT,
			fn get_ProvisioningInfo(&mut self, out: *mut *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceProvisioningInfo) -> ::w::HRESULT,
			fn get_SessionInfo(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWiFiDirectServiceSession, 2165580131, 58406, 18379, 134, 64, 225, 179, 88, 139, 242, 111);
		RT_INTERFACE!{interface IWiFiDirectServiceSession(IWiFiDirectServiceSessionVtbl): IInspectable(IInspectableVtbl) [IID_IWiFiDirectServiceSession] {
			fn get_ServiceName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSessionStatus) -> ::w::HRESULT,
			fn get_ErrorStatus(&mut self, out: *mut ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSessionErrorStatus) -> ::w::HRESULT,
			fn get_SessionId(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_AdvertisementId(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ServiceAddress(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SessionAddress(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetConnectionEndpointPairs(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::networking::EndpointPair>) -> ::w::HRESULT,
			fn add_SessionStatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_SessionStatusChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn AddStreamSocketListenerAsync(&mut self, value: *mut ::rt::gen::windows::networking::sockets::StreamSocketListener, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn AddDatagramSocketAsync(&mut self, value: *mut ::rt::gen::windows::networking::sockets::DatagramSocket, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn add_RemotePortAdded(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession, &::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceRemotePortAddedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_RemotePortAdded(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
} // Windows.Devices.WiFiDirect.Services
} // Windows.Devices.WiFiDirect
pub mod custom { // Windows.Devices.Custom
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_STRUCT! { struct CustomDeviceContract {
			
		}}
		DEFINE_IID!(IID_IKnownDeviceTypesStatics, 3998513602, 21576, 17882, 173, 27, 36, 148, 140, 35, 144, 148);
		RT_INTERFACE!{interface IKnownDeviceTypesStatics(IKnownDeviceTypesStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IKnownDeviceTypesStatics] {
			fn get_Unknown(&mut self, out: *mut u16) -> ::w::HRESULT
		}}
		RT_ENUM! { enum IOControlAccessMode: i32 {
			Any (IOControlAccessMode_Any) = 0, Read (IOControlAccessMode_Read) = 1, Write (IOControlAccessMode_Write) = 2, ReadWrite (IOControlAccessMode_ReadWrite) = 3,
		}}
		RT_ENUM! { enum IOControlBufferingMethod: i32 {
			Buffered (IOControlBufferingMethod_Buffered) = 0, DirectInput (IOControlBufferingMethod_DirectInput) = 1, DirectOutput (IOControlBufferingMethod_DirectOutput) = 2, Neither (IOControlBufferingMethod_Neither) = 3,
		}}
		DEFINE_IID!(IID_IIOControlCode, 244668903, 24776, 17269, 167, 97, 127, 136, 8, 6, 108, 96);
		RT_INTERFACE!{interface IIOControlCode(IIOControlCodeVtbl): IInspectable(IInspectableVtbl) [IID_IIOControlCode] {
			fn get_AccessMode(&mut self, out: *mut ::rt::gen::windows::devices::custom::IOControlAccessMode) -> ::w::HRESULT,
			fn get_BufferingMethod(&mut self, out: *mut ::rt::gen::windows::devices::custom::IOControlBufferingMethod) -> ::w::HRESULT,
			fn get_Function(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_DeviceType(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn get_ControlCode(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IIOControlCodeFactory, 2238348528, 19473, 17582, 175, 198, 184, 212, 162, 18, 120, 143);
		RT_INTERFACE!{interface IIOControlCodeFactory(IIOControlCodeFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IIOControlCodeFactory] {
			fn CreateIOControlCode(&mut self, deviceType: u16, function: u16, accessMode: ::rt::gen::windows::devices::custom::IOControlAccessMode, bufferingMethod: ::rt::gen::windows::devices::custom::IOControlBufferingMethod, out: *mut *mut ::rt::gen::windows::devices::custom::IOControlCode) -> ::w::HRESULT
		}}
		RT_CLASS!(IOControlCode: ::rt::gen::windows::devices::custom::IIOControlCode);
		RT_ENUM! { enum DeviceAccessMode: i32 {
			Read (DeviceAccessMode_Read) = 0, Write (DeviceAccessMode_Write) = 1, ReadWrite (DeviceAccessMode_ReadWrite) = 2,
		}}
		RT_ENUM! { enum DeviceSharingMode: i32 {
			Shared (DeviceSharingMode_Shared) = 0, Exclusive (DeviceSharingMode_Exclusive) = 1,
		}}
		DEFINE_IID!(IID_ICustomDeviceStatics, 3357672210, 61260, 18097, 165, 142, 238, 179, 8, 220, 137, 23);
		RT_INTERFACE!{interface ICustomDeviceStatics(ICustomDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_ICustomDeviceStatics] {
			fn GetDeviceSelector(&mut self, classGuid: ::w::GUID, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, desiredAccess: ::rt::gen::windows::devices::custom::DeviceAccessMode, sharingMode: ::rt::gen::windows::devices::custom::DeviceSharingMode, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::custom::CustomDevice>) -> ::w::HRESULT
		}}
		RT_CLASS!(CustomDevice: ::rt::gen::windows::devices::custom::ICustomDevice);
		DEFINE_IID!(IID_ICustomDevice, 3710919967, 50315, 17341, 188, 177, 222, 200, 143, 21, 20, 62);
		RT_INTERFACE!{interface ICustomDevice(ICustomDeviceVtbl): IInspectable(IInspectableVtbl) [IID_ICustomDevice] {
			fn get_InputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IInputStream) -> ::w::HRESULT,
			fn get_OutputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IOutputStream) -> ::w::HRESULT,
			fn SendIOControlAsync(&mut self, ioControlCode: *mut ::rt::gen::windows::devices::custom::IIOControlCode, inputBuffer: *mut ::rt::gen::windows::storage::streams::IBuffer, outputBuffer: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<u32>) -> ::w::HRESULT,
			fn TrySendIOControlAsync(&mut self, ioControlCode: *mut ::rt::gen::windows::devices::custom::IIOControlCode, inputBuffer: *mut ::rt::gen::windows::storage::streams::IBuffer, outputBuffer: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT
		}}
} // Windows.Devices.Custom
pub mod portable { // Windows.Devices.Portable
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_STRUCT! { struct PortableDeviceContract {
			
		}}
		RT_ENUM! { enum ServiceDeviceType: i32 {
			CalendarService (ServiceDeviceType_CalendarService) = 0, ContactsService (ServiceDeviceType_ContactsService) = 1, DeviceStatusService (ServiceDeviceType_DeviceStatusService) = 2, NotesService (ServiceDeviceType_NotesService) = 3, RingtonesService (ServiceDeviceType_RingtonesService) = 4, SmsService (ServiceDeviceType_SmsService) = 5, TasksService (ServiceDeviceType_TasksService) = 6,
		}}
		DEFINE_IID!(IID_IStorageDeviceStatics, 1590576366, 6947, 19922, 134, 82, 188, 22, 79, 0, 49, 40);
		RT_INTERFACE!{interface IStorageDeviceStatics(IStorageDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IStorageDeviceStatics] {
			fn FromId(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::storage::StorageFolder) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IServiceDeviceStatics, 2827097313, 22983, 18976, 171, 166, 159, 103, 7, 147, 114, 48);
		RT_INTERFACE!{interface IServiceDeviceStatics(IServiceDeviceStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IServiceDeviceStatics] {
			fn GetDeviceSelector(&mut self, serviceType: ::rt::gen::windows::devices::portable::ServiceDeviceType, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetDeviceSelectorFromServiceId(&mut self, serviceId: ::w::GUID, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
} // Windows.Devices.Portable
pub mod scanners { // Windows.Devices.Scanners
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_STRUCT! { struct ScannerDeviceContract {
			
		}}
		RT_ENUM! { enum ImageScannerFormat: i32 {
			Jpeg (ImageScannerFormat_Jpeg) = 0, Png (ImageScannerFormat_Png) = 1, DeviceIndependentBitmap (ImageScannerFormat_DeviceIndependentBitmap) = 2, Tiff (ImageScannerFormat_Tiff) = 3, Xps (ImageScannerFormat_Xps) = 4, OpenXps (ImageScannerFormat_OpenXps) = 5, Pdf (ImageScannerFormat_Pdf) = 6,
		}}
		DEFINE_IID!(IID_IImageScannerFormatConfiguration, 2921815313, 56031, 16400, 191, 16, 204, 165, 200, 61, 203, 176);
		RT_INTERFACE!{interface IImageScannerFormatConfiguration(IImageScannerFormatConfigurationVtbl): IInspectable(IInspectableVtbl) [IID_IImageScannerFormatConfiguration] {
			fn get_DefaultFormat(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerFormat) -> ::w::HRESULT,
			fn get_Format(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerFormat) -> ::w::HRESULT,
			fn put_Format(&mut self, value: ::rt::gen::windows::devices::scanners::ImageScannerFormat) -> ::w::HRESULT,
			fn IsFormatSupported(&mut self, value: ::rt::gen::windows::devices::scanners::ImageScannerFormat, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(ImageScannerAutoConfiguration: ::rt::gen::windows::devices::scanners::IImageScannerFormatConfiguration);
		RT_ENUM! { enum ImageScannerAutoCroppingMode: i32 {
			Disabled (ImageScannerAutoCroppingMode_Disabled) = 0, SingleRegion (ImageScannerAutoCroppingMode_SingleRegion) = 1, MultipleRegion (ImageScannerAutoCroppingMode_MultipleRegion) = 2,
		}}
		RT_ENUM! { enum ImageScannerColorMode: i32 {
			Color (ImageScannerColorMode_Color) = 0, Grayscale (ImageScannerColorMode_Grayscale) = 1, Monochrome (ImageScannerColorMode_Monochrome) = 2, AutoColor (ImageScannerColorMode_AutoColor) = 3,
		}}
		RT_STRUCT! { struct ImageScannerResolution {
			DpiX: f32, DpiY: f32,
		}}
		DEFINE_IID!(IID_IImageScannerSourceConfiguration, 3216310357, 2884, 19586, 158, 137, 32, 95, 156, 35, 78, 89);
		RT_INTERFACE!{interface IImageScannerSourceConfiguration(IImageScannerSourceConfigurationVtbl): IInspectable(IInspectableVtbl) [IID_IImageScannerSourceConfiguration] {
			fn get_MinScanArea(&mut self, out: *mut ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn get_MaxScanArea(&mut self, out: *mut ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn get_SelectedScanRegion(&mut self, out: *mut ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn put_SelectedScanRegion(&mut self, value: ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn get_AutoCroppingMode(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerAutoCroppingMode) -> ::w::HRESULT,
			fn put_AutoCroppingMode(&mut self, value: ::rt::gen::windows::devices::scanners::ImageScannerAutoCroppingMode) -> ::w::HRESULT,
			fn IsAutoCroppingModeSupported(&mut self, value: ::rt::gen::windows::devices::scanners::ImageScannerAutoCroppingMode, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_MinResolution(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerResolution) -> ::w::HRESULT,
			fn get_MaxResolution(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerResolution) -> ::w::HRESULT,
			fn get_OpticalResolution(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerResolution) -> ::w::HRESULT,
			fn get_DesiredResolution(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerResolution) -> ::w::HRESULT,
			fn put_DesiredResolution(&mut self, value: ::rt::gen::windows::devices::scanners::ImageScannerResolution) -> ::w::HRESULT,
			fn get_ActualResolution(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerResolution) -> ::w::HRESULT,
			fn get_DefaultColorMode(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerColorMode) -> ::w::HRESULT,
			fn get_ColorMode(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerColorMode) -> ::w::HRESULT,
			fn put_ColorMode(&mut self, value: ::rt::gen::windows::devices::scanners::ImageScannerColorMode) -> ::w::HRESULT,
			fn IsColorModeSupported(&mut self, value: ::rt::gen::windows::devices::scanners::ImageScannerColorMode, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_MinBrightness(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MaxBrightness(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_BrightnessStep(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_DefaultBrightness(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Brightness(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_Brightness(&mut self, value: i32) -> ::w::HRESULT,
			fn get_MinContrast(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_MaxContrast(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_ContrastStep(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_DefaultContrast(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Contrast(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_Contrast(&mut self, value: i32) -> ::w::HRESULT
		}}
		RT_CLASS!(ImageScannerFlatbedConfiguration: ::rt::gen::windows::devices::scanners::IImageScannerFormatConfiguration);
		DEFINE_IID!(IID_IImageScannerFeederConfiguration, 1958587630, 64151, 19479, 130, 128, 64, 227, 156, 109, 204, 103);
		RT_INTERFACE!{interface IImageScannerFeederConfiguration(IImageScannerFeederConfigurationVtbl): IInspectable(IInspectableVtbl) [IID_IImageScannerFeederConfiguration] {
			fn get_CanAutoDetectPageSize(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_AutoDetectPageSize(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_AutoDetectPageSize(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_PageSize(&mut self, out: *mut ::rt::gen::windows::graphics::printing::PrintMediaSize) -> ::w::HRESULT,
			fn put_PageSize(&mut self, value: ::rt::gen::windows::graphics::printing::PrintMediaSize) -> ::w::HRESULT,
			fn get_PageOrientation(&mut self, out: *mut ::rt::gen::windows::graphics::printing::PrintOrientation) -> ::w::HRESULT,
			fn put_PageOrientation(&mut self, value: ::rt::gen::windows::graphics::printing::PrintOrientation) -> ::w::HRESULT,
			fn get_PageSizeDimensions(&mut self, out: *mut ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn IsPageSizeSupported(&mut self, pageSize: ::rt::gen::windows::graphics::printing::PrintMediaSize, pageOrientation: ::rt::gen::windows::graphics::printing::PrintOrientation, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_MaxNumberOfPages(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_MaxNumberOfPages(&mut self, value: u32) -> ::w::HRESULT,
			fn get_CanScanDuplex(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Duplex(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_Duplex(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_CanScanAhead(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_ScanAhead(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_ScanAhead(&mut self, value: ::w::BOOL) -> ::w::HRESULT
		}}
		RT_CLASS!(ImageScannerFeederConfiguration: ::rt::gen::windows::devices::scanners::IImageScannerFormatConfiguration);
		DEFINE_IID!(IID_IImageScannerScanResult, 3373671629, 36919, 20040, 132, 193, 172, 9, 117, 7, 107, 197);
		RT_INTERFACE!{interface IImageScannerScanResult(IImageScannerScanResultVtbl): IInspectable(IInspectableVtbl) [IID_IImageScannerScanResult] {
			fn get_ScannedFiles(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT
		}}
		RT_CLASS!(ImageScannerScanResult: ::rt::gen::windows::devices::scanners::IImageScannerScanResult);
		DEFINE_IID!(IID_IImageScannerPreviewResult, 146275982, 34961, 17437, 190, 156, 23, 111, 161, 9, 200, 187);
		RT_INTERFACE!{interface IImageScannerPreviewResult(IImageScannerPreviewResultVtbl): IInspectable(IInspectableVtbl) [IID_IImageScannerPreviewResult] {
			fn get_Succeeded(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Format(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerFormat) -> ::w::HRESULT
		}}
		RT_CLASS!(ImageScannerPreviewResult: ::rt::gen::windows::devices::scanners::IImageScannerPreviewResult);
		RT_ENUM! { enum ImageScannerScanSource: i32 {
			Default (ImageScannerScanSource_Default) = 0, Flatbed (ImageScannerScanSource_Flatbed) = 1, Feeder (ImageScannerScanSource_Feeder) = 2, AutoConfigured (ImageScannerScanSource_AutoConfigured) = 3,
		}}
		DEFINE_IID!(IID_IImageScanner, 1403555704, 21144, 18592, 141, 163, 128, 135, 81, 150, 101, 224);
		RT_INTERFACE!{interface IImageScanner(IImageScannerVtbl): IInspectable(IInspectableVtbl) [IID_IImageScanner] {
			fn get_DeviceId(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DefaultScanSource(&mut self, out: *mut ::rt::gen::windows::devices::scanners::ImageScannerScanSource) -> ::w::HRESULT,
			fn IsScanSourceSupported(&mut self, value: ::rt::gen::windows::devices::scanners::ImageScannerScanSource, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_FlatbedConfiguration(&mut self, out: *mut *mut ::rt::gen::windows::devices::scanners::ImageScannerFlatbedConfiguration) -> ::w::HRESULT,
			fn get_FeederConfiguration(&mut self, out: *mut *mut ::rt::gen::windows::devices::scanners::ImageScannerFeederConfiguration) -> ::w::HRESULT,
			fn get_AutoConfiguration(&mut self, out: *mut *mut ::rt::gen::windows::devices::scanners::ImageScannerAutoConfiguration) -> ::w::HRESULT,
			fn IsPreviewSupported(&mut self, scanSource: ::rt::gen::windows::devices::scanners::ImageScannerScanSource, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn ScanPreviewToStreamAsync(&mut self, scanSource: ::rt::gen::windows::devices::scanners::ImageScannerScanSource, targetStream: *mut ::rt::gen::windows::storage::streams::IRandomAccessStream, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::scanners::ImageScannerPreviewResult>) -> ::w::HRESULT,
			fn ScanFilesToFolderAsync(&mut self, scanSource: ::rt::gen::windows::devices::scanners::ImageScannerScanSource, storageFolder: *mut ::rt::gen::windows::storage::StorageFolder, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperationWithProgress<&::rt::gen::windows::devices::scanners::ImageScannerScanResult, u32>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IImageScannerStatics, 3159877390, 55300, 17527, 159, 181, 185, 17, 181, 71, 56, 151);
		RT_INTERFACE!{interface IImageScannerStatics(IImageScannerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IImageScannerStatics] {
			fn FromIdAsync(&mut self, deviceId: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::devices::scanners::ImageScanner>) -> ::w::HRESULT,
			fn GetDeviceSelector(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(ImageScanner: ::rt::gen::windows::devices::scanners::IImageScanner);
} // Windows.Devices.Scanners
} // Windows.Devices
pub mod storage { // Windows.Storage
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum FileAccessMode: i32 {
			Read (FileAccessMode_Read) = 0, ReadWrite (FileAccessMode_ReadWrite) = 1,
		}}
		RT_CLASS!(StorageFolder: ::rt::gen::windows::storage::IStorageFolder);
		DEFINE_IID!(IID_IStorageFolder, 1926351736, 46063, 20341, 168, 11, 111, 217, 218, 226, 148, 75);
		RT_INTERFACE!{interface IStorageFolder(IStorageFolderVtbl): IInspectable(IInspectableVtbl) [IID_IStorageFolder] {
			fn CreateFileAsyncOverloadDefaultOptions(&mut self, desiredName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT,
			fn CreateFileAsync(&mut self, desiredName: ::w::HSTRING, options: ::rt::gen::windows::storage::CreationCollisionOption, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT,
			fn CreateFolderAsyncOverloadDefaultOptions(&mut self, desiredName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFolder>) -> ::w::HRESULT,
			fn CreateFolderAsync(&mut self, desiredName: ::w::HSTRING, options: ::rt::gen::windows::storage::CreationCollisionOption, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFolder>) -> ::w::HRESULT,
			fn GetFileAsync(&mut self, name: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT,
			fn GetFolderAsync(&mut self, name: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFolder>) -> ::w::HRESULT,
			fn GetItemAsync(&mut self, name: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::IStorageItem>) -> ::w::HRESULT,
			fn GetFilesAsyncOverloadDefaultOptionsStartAndCount(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::storage::StorageFile>>) -> ::w::HRESULT,
			fn GetFoldersAsyncOverloadDefaultOptionsStartAndCount(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::storage::StorageFolder>>) -> ::w::HRESULT,
			fn GetItemsAsyncOverloadDefaultStartAndCount(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::storage::IStorageItem>>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IStorageItem, 1107798422, 51759, 17143, 189, 232, 139, 16, 69, 122, 127, 48);
		RT_INTERFACE!{interface IStorageItem(IStorageItemVtbl): IInspectable(IInspectableVtbl) [IID_IStorageItem] {
			fn RenameAsyncOverloadDefaultOptions(&mut self, desiredName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn RenameAsync(&mut self, desiredName: ::w::HSTRING, option: ::rt::gen::windows::storage::NameCollisionOption, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn DeleteAsyncOverloadDefaultOptions(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn DeleteAsync(&mut self, option: ::rt::gen::windows::storage::StorageDeleteOption, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn GetBasicPropertiesAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::fileproperties::BasicProperties>) -> ::w::HRESULT,
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Path(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Attributes(&mut self, out: *mut ::rt::gen::windows::storage::FileAttributes) -> ::w::HRESULT,
			fn get_DateCreated(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn IsOfType(&mut self, type_: ::rt::gen::windows::storage::StorageItemTypes, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_ENUM! { enum StorageItemTypes: u32 {
			None (StorageItemTypes_None) = 0, File (StorageItemTypes_File) = 1, Folder (StorageItemTypes_Folder) = 2,
		}}
		RT_ENUM! { enum FileAttributes: u32 {
			Normal (FileAttributes_Normal) = 0, ReadOnly (FileAttributes_ReadOnly) = 1, Directory (FileAttributes_Directory) = 16, Archive (FileAttributes_Archive) = 32, Temporary (FileAttributes_Temporary) = 256, LocallyIncomplete (FileAttributes_LocallyIncomplete) = 512,
		}}
		RT_ENUM! { enum StorageDeleteOption: i32 {
			Default (StorageDeleteOption_Default) = 0, PermanentDelete (StorageDeleteOption_PermanentDelete) = 1,
		}}
		RT_ENUM! { enum NameCollisionOption: i32 {
			GenerateUniqueName (NameCollisionOption_GenerateUniqueName) = 0, ReplaceExisting (NameCollisionOption_ReplaceExisting) = 1, FailIfExists (NameCollisionOption_FailIfExists) = 2,
		}}
		RT_ENUM! { enum CreationCollisionOption: i32 {
			GenerateUniqueName (CreationCollisionOption_GenerateUniqueName) = 0, ReplaceExisting (CreationCollisionOption_ReplaceExisting) = 1, FailIfExists (CreationCollisionOption_FailIfExists) = 2, OpenIfExists (CreationCollisionOption_OpenIfExists) = 3,
		}}
		RT_CLASS!(StorageFile: ::rt::gen::windows::storage::IStorageFile);
		DEFINE_IID!(IID_IStorageFile, 4198457734, 16916, 17036, 166, 76, 20, 201, 172, 115, 21, 234);
		RT_INTERFACE!{interface IStorageFile(IStorageFileVtbl): IInspectable(IInspectableVtbl) [IID_IStorageFile] {
			fn get_FileType(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_ContentType(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn OpenAsync(&mut self, accessMode: ::rt::gen::windows::storage::FileAccessMode, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IRandomAccessStream>) -> ::w::HRESULT,
			fn OpenTransactedWriteAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageStreamTransaction>) -> ::w::HRESULT,
			fn CopyOverloadDefaultNameAndOptions(&mut self, destinationFolder: *mut ::rt::gen::windows::storage::IStorageFolder, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT,
			fn CopyOverloadDefaultOptions(&mut self, destinationFolder: *mut ::rt::gen::windows::storage::IStorageFolder, desiredNewName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT,
			fn CopyOverload(&mut self, destinationFolder: *mut ::rt::gen::windows::storage::IStorageFolder, desiredNewName: ::w::HSTRING, option: ::rt::gen::windows::storage::NameCollisionOption, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT,
			fn CopyAndReplaceAsync(&mut self, fileToReplace: *mut ::rt::gen::windows::storage::IStorageFile, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn MoveOverloadDefaultNameAndOptions(&mut self, destinationFolder: *mut ::rt::gen::windows::storage::IStorageFolder, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn MoveOverloadDefaultOptions(&mut self, destinationFolder: *mut ::rt::gen::windows::storage::IStorageFolder, desiredNewName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn MoveOverload(&mut self, destinationFolder: *mut ::rt::gen::windows::storage::IStorageFolder, desiredNewName: ::w::HSTRING, option: ::rt::gen::windows::storage::NameCollisionOption, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn MoveAndReplaceAsync(&mut self, fileToReplace: *mut ::rt::gen::windows::storage::IStorageFile, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT
		}}
		RT_CLASS!(StorageStreamTransaction: ::rt::gen::windows::storage::IStorageStreamTransaction);
		DEFINE_IID!(IID_IStorageStreamTransaction, 4135383907, 42301, 19860, 174, 44, 103, 35, 45, 147, 172, 221);
		RT_INTERFACE!{interface IStorageStreamTransaction(IStorageStreamTransactionVtbl): IInspectable(IInspectableVtbl) [IID_IStorageStreamTransaction] {
			fn get_Stream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IRandomAccessStream) -> ::w::HRESULT,
			fn CommitAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT
		}}
pub mod streams { // Windows.Storage.Streams
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_IRandomAccessStreamWithContentType, 3424995367, 19261, 17295, 146, 50, 16, 199, 107, 199, 224, 56);
		RT_INTERFACE!{interface IRandomAccessStreamWithContentType(IRandomAccessStreamWithContentTypeVtbl): IInspectable(IInspectableVtbl) [IID_IRandomAccessStreamWithContentType] {
			
		}}
		DEFINE_IID!(IID_IBuffer, 2421821408, 48211, 4575, 140, 73, 0, 30, 79, 198, 134, 218);
		RT_INTERFACE!{interface IBuffer(IBufferVtbl): IInspectable(IInspectableVtbl) [IID_IBuffer] {
			fn get_Capacity(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Length(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_Length(&mut self, value: u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IOutputStream, 2421821414, 48211, 4575, 140, 73, 0, 30, 79, 198, 134, 218);
		RT_INTERFACE!{interface IOutputStream(IOutputStreamVtbl): IInspectable(IInspectableVtbl) [IID_IOutputStream] {
			fn WriteAsync(&mut self, buffer: *mut ::rt::gen::windows::storage::streams::IBuffer, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperationWithProgress<u32, u32>) -> ::w::HRESULT,
			fn FlushAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<bool>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IInputStream, 2421821410, 48211, 4575, 140, 73, 0, 30, 79, 198, 134, 218);
		RT_INTERFACE!{interface IInputStream(IInputStreamVtbl): IInspectable(IInspectableVtbl) [IID_IInputStream] {
			fn ReadAsync(&mut self, buffer: *mut ::rt::gen::windows::storage::streams::IBuffer, count: u32, options: ::rt::gen::windows::storage::streams::InputStreamOptions, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperationWithProgress<&::rt::gen::windows::storage::streams::IBuffer, u32>) -> ::w::HRESULT
		}}
		RT_ENUM! { enum InputStreamOptions: u32 {
			None (InputStreamOptions_None) = 0, Partial (InputStreamOptions_Partial) = 1, ReadAhead (InputStreamOptions_ReadAhead) = 2,
		}}
		RT_CLASS!(DataReader: ::rt::gen::windows::storage::streams::IDataReader);
		DEFINE_IID!(IID_IDataReader, 3803512873, 46273, 17172, 164, 184, 251, 129, 58, 47, 39, 94);
		RT_INTERFACE!{interface IDataReader(IDataReaderVtbl): IInspectable(IInspectableVtbl) [IID_IDataReader] {
			fn get_UnconsumedBufferLength(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_UnicodeEncoding(&mut self, out: *mut ::rt::gen::windows::storage::streams::UnicodeEncoding) -> ::w::HRESULT,
			fn put_UnicodeEncoding(&mut self, value: ::rt::gen::windows::storage::streams::UnicodeEncoding) -> ::w::HRESULT,
			fn get_ByteOrder(&mut self, out: *mut ::rt::gen::windows::storage::streams::ByteOrder) -> ::w::HRESULT,
			fn put_ByteOrder(&mut self, value: ::rt::gen::windows::storage::streams::ByteOrder) -> ::w::HRESULT,
			fn get_InputStreamOptions(&mut self, out: *mut ::rt::gen::windows::storage::streams::InputStreamOptions) -> ::w::HRESULT,
			fn put_InputStreamOptions(&mut self, value: ::rt::gen::windows::storage::streams::InputStreamOptions) -> ::w::HRESULT,
			fn ReadByte(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn ReadBytes(&mut self, value: *mut u8) -> ::w::HRESULT,
			fn ReadBuffer(&mut self, length: u32, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn ReadBoolean(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn ReadGuid(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn ReadInt16(&mut self, out: *mut i16) -> ::w::HRESULT,
			fn ReadInt32(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn ReadInt64(&mut self, out: *mut i64) -> ::w::HRESULT,
			fn ReadUInt16(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn ReadUInt32(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn ReadUInt64(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn ReadSingle(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn ReadDouble(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn ReadString(&mut self, codeUnitCount: u32, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn ReadDateTime(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn ReadTimeSpan(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn LoadAsync(&mut self, count: u32, out: *mut *mut ::rt::gen::windows::storage::streams::DataReaderLoadOperation) -> ::w::HRESULT,
			fn DetachBuffer(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn DetachStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IInputStream) -> ::w::HRESULT
		}}
		RT_CLASS!(DataReaderLoadOperation: ::rt::gen::windows::foundation::IAsyncOperation<u32>);
		RT_ENUM! { enum ByteOrder: i32 {
			LittleEndian (ByteOrder_LittleEndian) = 0, BigEndian (ByteOrder_BigEndian) = 1,
		}}
		RT_ENUM! { enum UnicodeEncoding: i32 {
			Utf8 (UnicodeEncoding_Utf8) = 0, Utf16LE (UnicodeEncoding_Utf16LE) = 1, Utf16BE (UnicodeEncoding_Utf16BE) = 2,
		}}
		DEFINE_IID!(IID_IRandomAccessStream, 2421821409, 48211, 4575, 140, 73, 0, 30, 79, 198, 134, 218);
		RT_INTERFACE!{interface IRandomAccessStream(IRandomAccessStreamVtbl): IInspectable(IInspectableVtbl) [IID_IRandomAccessStream] {
			fn get_Size(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn put_Size(&mut self, value: u64) -> ::w::HRESULT,
			fn GetInputStreamAt(&mut self, position: u64, out: *mut *mut ::rt::gen::windows::storage::streams::IInputStream) -> ::w::HRESULT,
			fn GetOutputStreamAt(&mut self, position: u64, out: *mut *mut ::rt::gen::windows::storage::streams::IOutputStream) -> ::w::HRESULT,
			fn get_Position(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn Seek(&mut self, position: u64) -> ::w::HRESULT,
			fn CloneStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IRandomAccessStream) -> ::w::HRESULT,
			fn get_CanRead(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_CanWrite(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
} // Windows.Storage.Streams
pub mod fileproperties { // Windows.Storage.FileProperties
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(BasicProperties: ::rt::gen::windows::storage::fileproperties::IBasicProperties);
		DEFINE_IID!(IID_IBasicProperties, 3495777755, 30814, 19046, 190, 2, 155, 238, 197, 138, 234, 129);
		RT_INTERFACE!{interface IBasicProperties(IBasicPropertiesVtbl): IInspectable(IInspectableVtbl) [IID_IBasicProperties] {
			fn get_Size(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn get_DateModified(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_ItemDate(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT
		}}
} // Windows.Storage.FileProperties
} // Windows.Storage
pub mod system { // Windows.System
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
pub mod power { // Windows.System.Power
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum BatteryStatus: i32 {
			NotPresent (BatteryStatus_NotPresent) = 0, Discharging (BatteryStatus_Discharging) = 1, Idle (BatteryStatus_Idle) = 2, Charging (BatteryStatus_Charging) = 3,
		}}
} // Windows.System.Power
} // Windows.System
pub mod security { // Windows.Security
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
pub mod credentials { // Windows.Security.Credentials
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(PasswordCredential: ::rt::gen::windows::security::credentials::IPasswordCredential);
		DEFINE_IID!(IID_IPasswordCredential, 1790019977, 50976, 16807, 166, 193, 254, 173, 179, 99, 41, 160);
		RT_INTERFACE!{interface IPasswordCredential(IPasswordCredentialVtbl): IInspectable(IInspectableVtbl) [IID_IPasswordCredential] {
			fn get_Resource(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_Resource(&mut self, resource: ::w::HSTRING) -> ::w::HRESULT,
			fn get_UserName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_UserName(&mut self, userName: ::w::HSTRING) -> ::w::HRESULT,
			fn get_Password(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_Password(&mut self, password: ::w::HSTRING) -> ::w::HRESULT,
			fn RetrievePassword(&mut self) -> ::w::HRESULT,
			fn get_Properties(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IPropertySet) -> ::w::HRESULT
		}}
} // Windows.Security.Credentials
pub mod cryptography { // Windows.Security.Cryptography
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
pub mod certificates { // Windows.Security.Cryptography.Certificates
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(Certificate: ::rt::gen::windows::security::cryptography::certificates::ICertificate);
		DEFINE_IID!(IID_ICertificate, 859796492, 1240, 17331, 178, 120, 140, 95, 204, 155, 229, 160);
		RT_INTERFACE!{interface ICertificate(ICertificateVtbl): IInspectable(IInspectableVtbl) [IID_ICertificate] {
			fn BuildChainAsync(&mut self, certificates: *mut ::rt::gen::windows::foundation::collections::IIterable<&::rt::gen::windows::security::cryptography::certificates::Certificate>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::security::cryptography::certificates::CertificateChain>) -> ::w::HRESULT,
			fn BuildChainWithParametersAsync(&mut self, certificates: *mut ::rt::gen::windows::foundation::collections::IIterable<&::rt::gen::windows::security::cryptography::certificates::Certificate>, parameters: *mut ::rt::gen::windows::security::cryptography::certificates::ChainBuildingParameters, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::security::cryptography::certificates::CertificateChain>) -> ::w::HRESULT,
			fn get_SerialNumber(&mut self, out: *mut *mut u8) -> ::w::HRESULT,
			fn GetHashValue(&mut self, out: *mut *mut u8) -> ::w::HRESULT,
			fn GetHashValueWithAlgorithm(&mut self, hashAlgorithmName: ::w::HSTRING, out: *mut *mut u8) -> ::w::HRESULT,
			fn GetCertificateBlob(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn get_Subject(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Issuer(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_HasPrivateKey(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_IsStronglyProtected(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_ValidFrom(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_ValidTo(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_EnhancedKeyUsages(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&str>) -> ::w::HRESULT,
			fn put_FriendlyName(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_FriendlyName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(ChainBuildingParameters: ::rt::gen::windows::security::cryptography::certificates::IChainBuildingParameters);
		DEFINE_IID!(IID_IChainBuildingParameters, 1110157602, 31885, 18359, 181, 155, 177, 39, 3, 115, 58, 195);
		RT_INTERFACE!{interface IChainBuildingParameters(IChainBuildingParametersVtbl): IInspectable(IInspectableVtbl) [IID_IChainBuildingParameters] {
			fn get_EnhancedKeyUsages(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&str>) -> ::w::HRESULT,
			fn get_ValidationTimestamp(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn put_ValidationTimestamp(&mut self, value: ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn get_RevocationCheckEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_RevocationCheckEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_NetworkRetrievalEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_NetworkRetrievalEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_AuthorityInformationAccessEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_AuthorityInformationAccessEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_CurrentTimeValidationEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_CurrentTimeValidationEnabled(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_ExclusiveTrustRoots(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVector<&::rt::gen::windows::security::cryptography::certificates::Certificate>) -> ::w::HRESULT
		}}
		RT_CLASS!(CertificateChain: ::rt::gen::windows::security::cryptography::certificates::ICertificateChain);
		DEFINE_IID!(IID_ICertificateChain, 549409669, 13969, 17665, 166, 44, 253, 151, 39, 139, 49, 238);
		RT_INTERFACE!{interface ICertificateChain(ICertificateChainVtbl): IInspectable(IInspectableVtbl) [IID_ICertificateChain] {
			fn Validate(&mut self, out: *mut ::rt::gen::windows::security::cryptography::certificates::ChainValidationResult) -> ::w::HRESULT,
			fn ValidateWithParameters(&mut self, parameter: *mut ::rt::gen::windows::security::cryptography::certificates::ChainValidationParameters, out: *mut ::rt::gen::windows::security::cryptography::certificates::ChainValidationResult) -> ::w::HRESULT,
			fn GetCertificates(&mut self, includeRoot: ::w::BOOL, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&::rt::gen::windows::security::cryptography::certificates::Certificate>) -> ::w::HRESULT
		}}
		RT_CLASS!(ChainValidationParameters: ::rt::gen::windows::security::cryptography::certificates::IChainValidationParameters);
		DEFINE_IID!(IID_IChainValidationParameters, 3295951690, 32432, 19286, 160, 64, 185, 200, 230, 85, 221, 243);
		RT_INTERFACE!{interface IChainValidationParameters(IChainValidationParametersVtbl): IInspectable(IInspectableVtbl) [IID_IChainValidationParameters] {
			fn get_CertificateChainPolicy(&mut self, out: *mut ::rt::gen::windows::security::cryptography::certificates::CertificateChainPolicy) -> ::w::HRESULT,
			fn put_CertificateChainPolicy(&mut self, value: ::rt::gen::windows::security::cryptography::certificates::CertificateChainPolicy) -> ::w::HRESULT,
			fn get_ServerDnsName(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn put_ServerDnsName(&mut self, value: *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT
		}}
		RT_ENUM! { enum CertificateChainPolicy: i32 {
			Base (CertificateChainPolicy_Base) = 0, Ssl (CertificateChainPolicy_Ssl) = 1, NTAuthentication (CertificateChainPolicy_NTAuthentication) = 2, MicrosoftRoot (CertificateChainPolicy_MicrosoftRoot) = 3,
		}}
		RT_ENUM! { enum ChainValidationResult: i32 {
			Success (ChainValidationResult_Success) = 0, Untrusted (ChainValidationResult_Untrusted) = 1, Revoked (ChainValidationResult_Revoked) = 2, Expired (ChainValidationResult_Expired) = 3, IncompleteChain (ChainValidationResult_IncompleteChain) = 4, InvalidSignature (ChainValidationResult_InvalidSignature) = 5, WrongUsage (ChainValidationResult_WrongUsage) = 6, InvalidName (ChainValidationResult_InvalidName) = 7, InvalidCertificateAuthorityPolicy (ChainValidationResult_InvalidCertificateAuthorityPolicy) = 8, BasicConstraintsError (ChainValidationResult_BasicConstraintsError) = 9, UnknownCriticalExtension (ChainValidationResult_UnknownCriticalExtension) = 10, RevocationInformationMissing (ChainValidationResult_RevocationInformationMissing) = 11, RevocationFailure (ChainValidationResult_RevocationFailure) = 12, OtherErrors (ChainValidationResult_OtherErrors) = 13,
		}}
} // Windows.Security.Cryptography.Certificates
} // Windows.Security.Cryptography
} // Windows.Security
pub mod networking { // Windows.Networking
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(HostName: ::rt::gen::windows::networking::IHostName);
		DEFINE_IID!(IID_IHostName, 3213806253, 60822, 18855, 144, 132, 212, 22, 202, 232, 141, 203);
		RT_INTERFACE!{interface IHostName(IHostNameVtbl): IInspectable(IInspectableVtbl) [IID_IHostName] {
			fn get_IPInformation(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::IPInformation) -> ::w::HRESULT,
			fn get_RawName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DisplayName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_CanonicalName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Type(&mut self, out: *mut ::rt::gen::windows::networking::HostNameType) -> ::w::HRESULT,
			fn IsEqual(&mut self, hostName: *mut ::rt::gen::windows::networking::HostName, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_ENUM! { enum HostNameType: i32 {
			DomainName (HostNameType_DomainName) = 0, Ipv4 (HostNameType_Ipv4) = 1, Ipv6 (HostNameType_Ipv6) = 2, Bluetooth (HostNameType_Bluetooth) = 3,
		}}
		RT_CLASS!(EndpointPair: ::rt::gen::windows::networking::IEndpointPair);
		DEFINE_IID!(IID_IEndpointPair, 866167350, 63738, 19248, 184, 86, 118, 81, 124, 59, 208, 109);
		RT_INTERFACE!{interface IEndpointPair(IEndpointPairVtbl): IInspectable(IInspectableVtbl) [IID_IEndpointPair] {
			fn get_LocalHostName(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn put_LocalHostName(&mut self, value: *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_LocalServiceName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_LocalServiceName(&mut self, value: ::w::HSTRING) -> ::w::HRESULT,
			fn get_RemoteHostName(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn put_RemoteHostName(&mut self, value: *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_RemoteServiceName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn put_RemoteServiceName(&mut self, value: ::w::HSTRING) -> ::w::HRESULT
		}}
pub mod connectivity { // Windows.Networking.Connectivity
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(IPInformation: ::rt::gen::windows::networking::connectivity::IIPInformation);
		DEFINE_IID!(IID_IIPInformation, 3629204960, 5007, 18391, 155, 58, 54, 187, 72, 140, 239, 51);
		RT_INTERFACE!{interface IIPInformation(IIPInformationVtbl): IInspectable(IInspectableVtbl) [IID_IIPInformation] {
			fn get_NetworkAdapter(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::NetworkAdapter) -> ::w::HRESULT,
			fn get_PrefixLength(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u8>) -> ::w::HRESULT
		}}
		RT_CLASS!(NetworkAdapter: ::rt::gen::windows::networking::connectivity::INetworkAdapter);
		DEFINE_IID!(IID_INetworkAdapter, 995372547, 21384, 18796, 168, 163, 175, 253, 57, 174, 194, 230);
		RT_INTERFACE!{interface INetworkAdapter(INetworkAdapterVtbl): IInspectable(IInspectableVtbl) [IID_INetworkAdapter] {
			fn get_OutboundMaxBitsPerSecond(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn get_InboundMaxBitsPerSecond(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn get_IanaInterfaceType(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_NetworkItem(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::NetworkItem) -> ::w::HRESULT,
			fn get_NetworkAdapterId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn GetConnectedProfileAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::networking::connectivity::ConnectionProfile>) -> ::w::HRESULT
		}}
		RT_CLASS!(ConnectionProfile: ::rt::gen::windows::networking::connectivity::IConnectionProfile);
		DEFINE_IID!(IID_IConnectionProfile, 1908020284, 22926, 18896, 132, 235, 143, 235, 174, 220, 193, 149);
		RT_INTERFACE!{interface IConnectionProfile(IConnectionProfileVtbl): IInspectable(IInspectableVtbl) [IID_IConnectionProfile] {
			fn get_ProfileName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetNetworkConnectivityLevel(&mut self, out: *mut ::rt::gen::windows::networking::connectivity::NetworkConnectivityLevel) -> ::w::HRESULT,
			fn GetNetworkNames(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<&str>) -> ::w::HRESULT,
			fn GetConnectionCost(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::ConnectionCost) -> ::w::HRESULT,
			fn GetDataPlanStatus(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::DataPlanStatus) -> ::w::HRESULT,
			fn get_NetworkAdapter(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::NetworkAdapter) -> ::w::HRESULT,
			fn GetLocalUsage(&mut self, startTime: ::rt::gen::windows::foundation::DateTime, endTime: ::rt::gen::windows::foundation::DateTime, out: *mut *mut ::rt::gen::windows::networking::connectivity::DataUsage) -> ::w::HRESULT,
			fn GetLocalUsagePerRoamingStates(&mut self, startTime: ::rt::gen::windows::foundation::DateTime, endTime: ::rt::gen::windows::foundation::DateTime, states: ::rt::gen::windows::networking::connectivity::RoamingStates, out: *mut *mut ::rt::gen::windows::networking::connectivity::DataUsage) -> ::w::HRESULT,
			fn get_NetworkSecuritySettings(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::NetworkSecuritySettings) -> ::w::HRESULT
		}}
		RT_CLASS!(NetworkSecuritySettings: ::rt::gen::windows::networking::connectivity::INetworkSecuritySettings);
		DEFINE_IID!(IID_INetworkSecuritySettings, 2090892941, 37243, 19295, 184, 77, 40, 247, 165, 172, 84, 2);
		RT_INTERFACE!{interface INetworkSecuritySettings(INetworkSecuritySettingsVtbl): IInspectable(IInspectableVtbl) [IID_INetworkSecuritySettings] {
			fn get_NetworkAuthenticationType(&mut self, out: *mut ::rt::gen::windows::networking::connectivity::NetworkAuthenticationType) -> ::w::HRESULT,
			fn get_NetworkEncryptionType(&mut self, out: *mut ::rt::gen::windows::networking::connectivity::NetworkEncryptionType) -> ::w::HRESULT
		}}
		RT_ENUM! { enum NetworkEncryptionType: i32 {
			None (NetworkEncryptionType_None) = 0, Unknown (NetworkEncryptionType_Unknown) = 1, Wep (NetworkEncryptionType_Wep) = 2, Wep40 (NetworkEncryptionType_Wep40) = 3, Wep104 (NetworkEncryptionType_Wep104) = 4, Tkip (NetworkEncryptionType_Tkip) = 5, Ccmp (NetworkEncryptionType_Ccmp) = 6, WpaUseGroup (NetworkEncryptionType_WpaUseGroup) = 7, RsnUseGroup (NetworkEncryptionType_RsnUseGroup) = 8, Ihv (NetworkEncryptionType_Ihv) = 9,
		}}
		RT_ENUM! { enum NetworkAuthenticationType: i32 {
			None (NetworkAuthenticationType_None) = 0, Unknown (NetworkAuthenticationType_Unknown) = 1, Open80211 (NetworkAuthenticationType_Open80211) = 2, SharedKey80211 (NetworkAuthenticationType_SharedKey80211) = 3, Wpa (NetworkAuthenticationType_Wpa) = 4, WpaPsk (NetworkAuthenticationType_WpaPsk) = 5, WpaNone (NetworkAuthenticationType_WpaNone) = 6, Rsna (NetworkAuthenticationType_Rsna) = 7, RsnaPsk (NetworkAuthenticationType_RsnaPsk) = 8, Ihv (NetworkAuthenticationType_Ihv) = 9,
		}}
		RT_ENUM! { enum RoamingStates: u32 {
			None (RoamingStates_None) = 0, NotRoaming (RoamingStates_NotRoaming) = 1, Roaming (RoamingStates_Roaming) = 2,
		}}
		RT_CLASS!(DataUsage: ::rt::gen::windows::networking::connectivity::IDataUsage);
		DEFINE_IID!(IID_IDataUsage, 3242401235, 45382, 19769, 185, 89, 12, 105, 176, 150, 197, 18);
		RT_INTERFACE!{interface IDataUsage(IDataUsageVtbl): IInspectable(IInspectableVtbl) [IID_IDataUsage] {
			fn get_BytesSent(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn get_BytesReceived(&mut self, out: *mut u64) -> ::w::HRESULT
		}}
		RT_CLASS!(DataPlanStatus: ::rt::gen::windows::networking::connectivity::IDataPlanStatus);
		DEFINE_IID!(IID_IDataPlanStatus, 2541390732, 14469, 16627, 136, 81, 66, 205, 43, 213, 104, 187);
		RT_INTERFACE!{interface IDataPlanStatus(IDataPlanStatusVtbl): IInspectable(IInspectableVtbl) [IID_IDataPlanStatus] {
			fn get_DataPlanUsage(&mut self, out: *mut *mut ::rt::gen::windows::networking::connectivity::DataPlanUsage) -> ::w::HRESULT,
			fn get_DataLimitInMegabytes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u32>) -> ::w::HRESULT,
			fn get_InboundBitsPerSecond(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u64>) -> ::w::HRESULT,
			fn get_OutboundBitsPerSecond(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u64>) -> ::w::HRESULT,
			fn get_NextBillingCycle(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<::rt::gen::windows::foundation::DateTime>) -> ::w::HRESULT,
			fn get_MaxTransferSizeInMegabytes(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IReference<u32>) -> ::w::HRESULT
		}}
		RT_CLASS!(DataPlanUsage: ::rt::gen::windows::networking::connectivity::IDataPlanUsage);
		DEFINE_IID!(IID_IDataPlanUsage, 3105966381, 15172, 18431, 179, 97, 190, 89, 230, 158, 209, 176);
		RT_INTERFACE!{interface IDataPlanUsage(IDataPlanUsageVtbl): IInspectable(IInspectableVtbl) [IID_IDataPlanUsage] {
			fn get_MegabytesUsed(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_LastSyncTime(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT
		}}
		RT_CLASS!(ConnectionCost: ::rt::gen::windows::networking::connectivity::IConnectionCost);
		DEFINE_IID!(IID_IConnectionCost, 3134707753, 13334, 19216, 162, 2, 186, 192, 176, 117, 189, 174);
		RT_INTERFACE!{interface IConnectionCost(IConnectionCostVtbl): IInspectable(IInspectableVtbl) [IID_IConnectionCost] {
			fn get_NetworkCostType(&mut self, out: *mut ::rt::gen::windows::networking::connectivity::NetworkCostType) -> ::w::HRESULT,
			fn get_Roaming(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_OverDataLimit(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_ApproachingDataLimit(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_ENUM! { enum NetworkCostType: i32 {
			Unknown (NetworkCostType_Unknown) = 0, Unrestricted (NetworkCostType_Unrestricted) = 1, Fixed (NetworkCostType_Fixed) = 2, Variable (NetworkCostType_Variable) = 3,
		}}
		RT_ENUM! { enum NetworkConnectivityLevel: i32 {
			None (NetworkConnectivityLevel_None) = 0, LocalAccess (NetworkConnectivityLevel_LocalAccess) = 1, ConstrainedInternetAccess (NetworkConnectivityLevel_ConstrainedInternetAccess) = 2, InternetAccess (NetworkConnectivityLevel_InternetAccess) = 3,
		}}
		RT_CLASS!(NetworkItem: ::rt::gen::windows::networking::connectivity::INetworkItem);
		DEFINE_IID!(IID_INetworkItem, 29117753, 62944, 17767, 162, 140, 66, 8, 12, 131, 27, 43);
		RT_INTERFACE!{interface INetworkItem(INetworkItemVtbl): IInspectable(IInspectableVtbl) [IID_INetworkItem] {
			fn get_NetworkId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn GetNetworkTypes(&mut self, out: *mut ::rt::gen::windows::networking::connectivity::NetworkTypes) -> ::w::HRESULT
		}}
		RT_ENUM! { enum NetworkTypes: u32 {
			None (NetworkTypes_None) = 0, Internet (NetworkTypes_Internet) = 1, PrivateNetwork (NetworkTypes_PrivateNetwork) = 2,
		}}
} // Windows.Networking.Connectivity
pub mod sockets { // Windows.Networking.Sockets
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum SocketSslErrorSeverity: i32 {
			None (SocketSslErrorSeverity_None) = 0, Ignorable (SocketSslErrorSeverity_Ignorable) = 1, Fatal (SocketSslErrorSeverity_Fatal) = 2,
		}}
		RT_ENUM! { enum SocketProtectionLevel: i32 {
			PlainSocket (SocketProtectionLevel_PlainSocket) = 0, Ssl (SocketProtectionLevel_Ssl) = 1, SslAllowNullEncryption (SocketProtectionLevel_SslAllowNullEncryption) = 2, BluetoothEncryptionAllowNullAuthentication (SocketProtectionLevel_BluetoothEncryptionAllowNullAuthentication) = 3, BluetoothEncryptionWithAuthentication (SocketProtectionLevel_BluetoothEncryptionWithAuthentication) = 4, Ssl3AllowWeakEncryption (SocketProtectionLevel_Ssl3AllowWeakEncryption) = 5, Tls10 (SocketProtectionLevel_Tls10) = 6, Tls11 (SocketProtectionLevel_Tls11) = 7, Tls12 (SocketProtectionLevel_Tls12) = 8,
		}}
		RT_CLASS!(StreamSocketListener: ::rt::gen::windows::networking::sockets::IStreamSocketListener);
		DEFINE_IID!(IID_IStreamSocketListener, 4283511863, 57247, 19952, 191, 130, 14, 197, 215, 179, 90, 174);
		RT_INTERFACE!{interface IStreamSocketListener(IStreamSocketListenerVtbl): IInspectable(IInspectableVtbl) [IID_IStreamSocketListener] {
			fn get_Control(&mut self, out: *mut *mut ::rt::gen::windows::networking::sockets::StreamSocketListenerControl) -> ::w::HRESULT,
			fn get_Information(&mut self, out: *mut *mut ::rt::gen::windows::networking::sockets::StreamSocketListenerInformation) -> ::w::HRESULT,
			fn BindServiceNameAsync(&mut self, localServiceName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn BindEndpointAsync(&mut self, localHostName: *mut ::rt::gen::windows::networking::HostName, localServiceName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn add_ConnectionReceived(&mut self, eventHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::networking::sockets::StreamSocketListener, &::rt::gen::windows::networking::sockets::StreamSocketListenerConnectionReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_ConnectionReceived(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(StreamSocketListenerConnectionReceivedEventArgs: ::rt::gen::windows::networking::sockets::IStreamSocketListenerConnectionReceivedEventArgs);
		DEFINE_IID!(IID_IStreamSocketListenerConnectionReceivedEventArgs, 205991593, 14143, 17531, 133, 177, 221, 212, 84, 136, 3, 186);
		RT_INTERFACE!{interface IStreamSocketListenerConnectionReceivedEventArgs(IStreamSocketListenerConnectionReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IStreamSocketListenerConnectionReceivedEventArgs] {
			fn get_Socket(&mut self, out: *mut *mut ::rt::gen::windows::networking::sockets::StreamSocket) -> ::w::HRESULT
		}}
		RT_CLASS!(StreamSocket: ::rt::gen::windows::networking::sockets::IStreamSocket);
		DEFINE_IID!(IID_IStreamSocket, 1772236019, 64635, 18519, 175, 56, 246, 231, 222, 106, 91, 73);
		RT_INTERFACE!{interface IStreamSocket(IStreamSocketVtbl): IInspectable(IInspectableVtbl) [IID_IStreamSocket] {
			fn get_Control(&mut self, out: *mut *mut ::rt::gen::windows::networking::sockets::StreamSocketControl) -> ::w::HRESULT,
			fn get_Information(&mut self, out: *mut *mut ::rt::gen::windows::networking::sockets::StreamSocketInformation) -> ::w::HRESULT,
			fn get_InputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IInputStream) -> ::w::HRESULT,
			fn get_OutputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IOutputStream) -> ::w::HRESULT,
			fn ConnectWithEndpointPairAsync(&mut self, endpointPair: *mut ::rt::gen::windows::networking::EndpointPair, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn ConnectAsync(&mut self, remoteHostName: *mut ::rt::gen::windows::networking::HostName, remoteServiceName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn ConnectWithEndpointPairAndProtectionLevelAsync(&mut self, endpointPair: *mut ::rt::gen::windows::networking::EndpointPair, protectionLevel: ::rt::gen::windows::networking::sockets::SocketProtectionLevel, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn ConnectWithProtectionLevelAsync(&mut self, remoteHostName: *mut ::rt::gen::windows::networking::HostName, remoteServiceName: ::w::HSTRING, protectionLevel: ::rt::gen::windows::networking::sockets::SocketProtectionLevel, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn UpgradeToSslAsync(&mut self, protectionLevel: ::rt::gen::windows::networking::sockets::SocketProtectionLevel, validationHostName: *mut ::rt::gen::windows::networking::HostName, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT
		}}
		RT_CLASS!(StreamSocketInformation: ::rt::gen::windows::networking::sockets::IStreamSocketInformation);
		DEFINE_IID!(IID_IStreamSocketInformation, 998288944, 24168, 16901, 136, 240, 220, 133, 210, 226, 93, 237);
		RT_INTERFACE!{interface IStreamSocketInformation(IStreamSocketInformationVtbl): IInspectable(IInspectableVtbl) [IID_IStreamSocketInformation] {
			fn get_LocalAddress(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_LocalPort(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_RemoteHostName(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_RemoteAddress(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_RemoteServiceName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_RemotePort(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_RoundTripTimeStatistics(&mut self, out: *mut ::rt::gen::windows::networking::sockets::RoundTripTimeStatistics) -> ::w::HRESULT,
			fn get_BandwidthStatistics(&mut self, out: *mut ::rt::gen::windows::networking::sockets::BandwidthStatistics) -> ::w::HRESULT,
			fn get_ProtectionLevel(&mut self, out: *mut ::rt::gen::windows::networking::sockets::SocketProtectionLevel) -> ::w::HRESULT,
			fn get_SessionKey(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT
		}}
		RT_STRUCT! { struct BandwidthStatistics {
			OutboundBitsPerSecond: u64, InboundBitsPerSecond: u64, OutboundBitsPerSecondInstability: u64, InboundBitsPerSecondInstability: u64, OutboundBandwidthPeaked: ::w::BOOL, InboundBandwidthPeaked: ::w::BOOL,
		}}
		RT_STRUCT! { struct RoundTripTimeStatistics {
			Variance: u32, Max: u32, Min: u32, Sum: u32,
		}}
		RT_CLASS!(StreamSocketControl: ::rt::gen::windows::networking::sockets::IStreamSocketControl);
		DEFINE_IID!(IID_IStreamSocketControl, 4263882225, 37547, 19187, 153, 146, 15, 76, 133, 227, 108, 196);
		RT_INTERFACE!{interface IStreamSocketControl(IStreamSocketControlVtbl): IInspectable(IInspectableVtbl) [IID_IStreamSocketControl] {
			fn get_NoDelay(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_NoDelay(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_KeepAlive(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_KeepAlive(&mut self, value: ::w::BOOL) -> ::w::HRESULT,
			fn get_OutboundBufferSizeInBytes(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_OutboundBufferSizeInBytes(&mut self, value: u32) -> ::w::HRESULT,
			fn get_QualityOfService(&mut self, out: *mut ::rt::gen::windows::networking::sockets::SocketQualityOfService) -> ::w::HRESULT,
			fn put_QualityOfService(&mut self, value: ::rt::gen::windows::networking::sockets::SocketQualityOfService) -> ::w::HRESULT,
			fn get_OutboundUnicastHopLimit(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn put_OutboundUnicastHopLimit(&mut self, value: u8) -> ::w::HRESULT
		}}
		RT_ENUM! { enum SocketQualityOfService: i32 {
			Normal (SocketQualityOfService_Normal) = 0, LowLatency (SocketQualityOfService_LowLatency) = 1,
		}}
		RT_CLASS!(StreamSocketListenerInformation: ::rt::gen::windows::networking::sockets::IStreamSocketListenerInformation);
		DEFINE_IID!(IID_IStreamSocketListenerInformation, 3861620783, 42554, 17163, 191, 98, 41, 233, 62, 86, 51, 180);
		RT_INTERFACE!{interface IStreamSocketListenerInformation(IStreamSocketListenerInformationVtbl): IInspectable(IInspectableVtbl) [IID_IStreamSocketListenerInformation] {
			fn get_LocalPort(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(StreamSocketListenerControl: ::rt::gen::windows::networking::sockets::IStreamSocketListenerControl);
		DEFINE_IID!(IID_IStreamSocketListenerControl, 551077238, 36234, 19898, 151, 34, 161, 108, 77, 152, 73, 128);
		RT_INTERFACE!{interface IStreamSocketListenerControl(IStreamSocketListenerControlVtbl): IInspectable(IInspectableVtbl) [IID_IStreamSocketListenerControl] {
			fn get_QualityOfService(&mut self, out: *mut ::rt::gen::windows::networking::sockets::SocketQualityOfService) -> ::w::HRESULT,
			fn put_QualityOfService(&mut self, value: ::rt::gen::windows::networking::sockets::SocketQualityOfService) -> ::w::HRESULT
		}}
		RT_CLASS!(DatagramSocket: ::rt::gen::windows::networking::sockets::IDatagramSocket);
		DEFINE_IID!(IID_IDatagramSocket, 2145541051, 50108, 18039, 132, 70, 202, 40, 164, 101, 163, 175);
		RT_INTERFACE!{interface IDatagramSocket(IDatagramSocketVtbl): IInspectable(IInspectableVtbl) [IID_IDatagramSocket] {
			fn get_Control(&mut self, out: *mut *mut ::rt::gen::windows::networking::sockets::DatagramSocketControl) -> ::w::HRESULT,
			fn get_Information(&mut self, out: *mut *mut ::rt::gen::windows::networking::sockets::DatagramSocketInformation) -> ::w::HRESULT,
			fn get_OutputStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IOutputStream) -> ::w::HRESULT,
			fn ConnectAsync(&mut self, remoteHostName: *mut ::rt::gen::windows::networking::HostName, remoteServiceName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn ConnectWithEndpointPairAsync(&mut self, endpointPair: *mut ::rt::gen::windows::networking::EndpointPair, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn BindServiceNameAsync(&mut self, localServiceName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn BindEndpointAsync(&mut self, localHostName: *mut ::rt::gen::windows::networking::HostName, localServiceName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn JoinMulticastGroup(&mut self, host: *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn GetOutputStreamAsync(&mut self, remoteHostName: *mut ::rt::gen::windows::networking::HostName, remoteServiceName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IOutputStream>) -> ::w::HRESULT,
			fn GetOutputStreamWithEndpointPairAsync(&mut self, endpointPair: *mut ::rt::gen::windows::networking::EndpointPair, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::streams::IOutputStream>) -> ::w::HRESULT,
			fn add_MessageReceived(&mut self, eventHandler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::networking::sockets::DatagramSocket, &::rt::gen::windows::networking::sockets::DatagramSocketMessageReceivedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_MessageReceived(&mut self, eventCookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(DatagramSocketMessageReceivedEventArgs: ::rt::gen::windows::networking::sockets::IDatagramSocketMessageReceivedEventArgs);
		DEFINE_IID!(IID_IDatagramSocketMessageReceivedEventArgs, 2653805730, 5906, 19684, 177, 121, 140, 101, 44, 109, 16, 126);
		RT_INTERFACE!{interface IDatagramSocketMessageReceivedEventArgs(IDatagramSocketMessageReceivedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IDatagramSocketMessageReceivedEventArgs] {
			fn get_RemoteAddress(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_RemotePort(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_LocalAddress(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn GetDataReader(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::DataReader) -> ::w::HRESULT,
			fn GetDataStream(&mut self, out: *mut *mut ::rt::gen::windows::storage::streams::IInputStream) -> ::w::HRESULT
		}}
		RT_CLASS!(DatagramSocketInformation: ::rt::gen::windows::networking::sockets::IDatagramSocketInformation);
		DEFINE_IID!(IID_IDatagramSocketInformation, 1595561626, 22011, 18637, 151, 6, 122, 151, 79, 123, 21, 133);
		RT_INTERFACE!{interface IDatagramSocketInformation(IDatagramSocketInformationVtbl): IInspectable(IInspectableVtbl) [IID_IDatagramSocketInformation] {
			fn get_LocalAddress(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_LocalPort(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_RemoteAddress(&mut self, out: *mut *mut ::rt::gen::windows::networking::HostName) -> ::w::HRESULT,
			fn get_RemotePort(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		RT_CLASS!(DatagramSocketControl: ::rt::gen::windows::networking::sockets::IDatagramSocketControl);
		DEFINE_IID!(IID_IDatagramSocketControl, 1387020078, 13466, 16693, 187, 88, 183, 155, 38, 71, 211, 144);
		RT_INTERFACE!{interface IDatagramSocketControl(IDatagramSocketControlVtbl): IInspectable(IInspectableVtbl) [IID_IDatagramSocketControl] {
			fn get_QualityOfService(&mut self, out: *mut ::rt::gen::windows::networking::sockets::SocketQualityOfService) -> ::w::HRESULT,
			fn put_QualityOfService(&mut self, value: ::rt::gen::windows::networking::sockets::SocketQualityOfService) -> ::w::HRESULT,
			fn get_OutboundUnicastHopLimit(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn put_OutboundUnicastHopLimit(&mut self, value: u8) -> ::w::HRESULT
		}}
} // Windows.Networking.Sockets
} // Windows.Networking
pub mod globalization { // Windows.Globalization
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(Language: ::rt::gen::windows::globalization::ILanguage);
		DEFINE_IID!(IID_ILanguage, 3933841234, 63426, 16997, 177, 189, 196, 222, 196, 228, 240, 128);
		RT_INTERFACE!{interface ILanguage(ILanguageVtbl): IInspectable(IInspectableVtbl) [IID_ILanguage] {
			fn get_LanguageTag(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DisplayName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_NativeName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Script(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
} // Windows.Globalization
pub mod ui { // Windows.UI
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_STRUCT! { struct Color {
			A: u8, R: u8, G: u8, B: u8,
		}}
pub mod popups { // Windows.UI.Popups
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum Placement: i32 {
			Default (Placement_Default) = 0, Above (Placement_Above) = 1, Below (Placement_Below) = 2, Left (Placement_Left) = 3, Right (Placement_Right) = 4,
		}}
} // Windows.UI.Popups
} // Windows.UI
pub mod applicationmodel { // Windows.ApplicationModel
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
pub mod background { // Windows.ApplicationModel.Background
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(DeviceWatcherTrigger: ::rt::gen::windows::applicationmodel::background::IDeviceWatcherTrigger);
		DEFINE_IID!(IID_IDeviceWatcherTrigger, 2757853149, 34163, 16992, 190, 252, 91, 236, 137, 203, 105, 61);
		RT_INTERFACE!{interface IDeviceWatcherTrigger(IDeviceWatcherTriggerVtbl): IInspectable(IInspectableVtbl) [IID_IDeviceWatcherTrigger] {
			
		}}
} // Windows.ApplicationModel.Background
} // Windows.ApplicationModel
pub mod graphics { // Windows.Graphics
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
pub mod imaging { // Windows.Graphics.Imaging
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum BitmapAlphaMode: i32 {
			Premultiplied (BitmapAlphaMode_Premultiplied) = 0, Straight (BitmapAlphaMode_Straight) = 1, Ignore (BitmapAlphaMode_Ignore) = 2,
		}}
		RT_ENUM! { enum BitmapPixelFormat: i32 {
			Unknown (BitmapPixelFormat_Unknown) = 0, Rgba16 (BitmapPixelFormat_Rgba16) = 12, Rgba8 (BitmapPixelFormat_Rgba8) = 30, Gray16 (BitmapPixelFormat_Gray16) = 57, Gray8 (BitmapPixelFormat_Gray8) = 62, Bgra8 (BitmapPixelFormat_Bgra8) = 87, Nv12 (BitmapPixelFormat_Nv12) = 103, Yuy2 (BitmapPixelFormat_Yuy2) = 107,
		}}
		RT_CLASS!(BitmapFrame: ::rt::gen::windows::graphics::imaging::IBitmapFrame);
		DEFINE_IID!(IID_IBitmapFrame, 1923389980, 32897, 17293, 145, 188, 148, 236, 252, 129, 133, 198);
		RT_INTERFACE!{interface IBitmapFrame(IBitmapFrameVtbl): IInspectable(IInspectableVtbl) [IID_IBitmapFrame] {
			fn GetThumbnailAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::graphics::imaging::ImageStream>) -> ::w::HRESULT,
			fn get_BitmapProperties(&mut self, out: *mut *mut ::rt::gen::windows::graphics::imaging::BitmapPropertiesView) -> ::w::HRESULT,
			fn get_BitmapPixelFormat(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapPixelFormat) -> ::w::HRESULT,
			fn get_BitmapAlphaMode(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapAlphaMode) -> ::w::HRESULT,
			fn get_DpiX(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_DpiY(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn get_PixelWidth(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_PixelHeight(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_OrientedPixelWidth(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_OrientedPixelHeight(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn GetPixelDataAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::graphics::imaging::PixelDataProvider>) -> ::w::HRESULT,
			fn GetPixelDataTransformedAsync(&mut self, pixelFormat: ::rt::gen::windows::graphics::imaging::BitmapPixelFormat, alphaMode: ::rt::gen::windows::graphics::imaging::BitmapAlphaMode, transform: *mut ::rt::gen::windows::graphics::imaging::BitmapTransform, exifOrientationMode: ::rt::gen::windows::graphics::imaging::ExifOrientationMode, colorManagementMode: ::rt::gen::windows::graphics::imaging::ColorManagementMode, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::graphics::imaging::PixelDataProvider>) -> ::w::HRESULT
		}}
		RT_ENUM! { enum ColorManagementMode: i32 {
			DoNotColorManage (ColorManagementMode_DoNotColorManage) = 0, ColorManageToSRgb (ColorManagementMode_ColorManageToSRgb) = 1,
		}}
		RT_ENUM! { enum ExifOrientationMode: i32 {
			IgnoreExifOrientation (ExifOrientationMode_IgnoreExifOrientation) = 0, RespectExifOrientation (ExifOrientationMode_RespectExifOrientation) = 1,
		}}
		RT_CLASS!(BitmapTransform: ::rt::gen::windows::graphics::imaging::IBitmapTransform);
		DEFINE_IID!(IID_IBitmapTransform, 2926924612, 57960, 19765, 173, 207, 233, 149, 211, 26, 141, 52);
		RT_INTERFACE!{interface IBitmapTransform(IBitmapTransformVtbl): IInspectable(IInspectableVtbl) [IID_IBitmapTransform] {
			fn get_ScaledWidth(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ScaledWidth(&mut self, value: u32) -> ::w::HRESULT,
			fn get_ScaledHeight(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn put_ScaledHeight(&mut self, value: u32) -> ::w::HRESULT,
			fn get_InterpolationMode(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapInterpolationMode) -> ::w::HRESULT,
			fn put_InterpolationMode(&mut self, value: ::rt::gen::windows::graphics::imaging::BitmapInterpolationMode) -> ::w::HRESULT,
			fn get_Flip(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapFlip) -> ::w::HRESULT,
			fn put_Flip(&mut self, value: ::rt::gen::windows::graphics::imaging::BitmapFlip) -> ::w::HRESULT,
			fn get_Rotation(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapRotation) -> ::w::HRESULT,
			fn put_Rotation(&mut self, value: ::rt::gen::windows::graphics::imaging::BitmapRotation) -> ::w::HRESULT,
			fn get_Bounds(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapBounds) -> ::w::HRESULT,
			fn put_Bounds(&mut self, value: ::rt::gen::windows::graphics::imaging::BitmapBounds) -> ::w::HRESULT
		}}
		RT_STRUCT! { struct BitmapBounds {
			X: u32, Y: u32, Width: u32, Height: u32,
		}}
		RT_ENUM! { enum BitmapRotation: i32 {
			None (BitmapRotation_None) = 0, Clockwise90Degrees (BitmapRotation_Clockwise90Degrees) = 1, Clockwise180Degrees (BitmapRotation_Clockwise180Degrees) = 2, Clockwise270Degrees (BitmapRotation_Clockwise270Degrees) = 3,
		}}
		RT_ENUM! { enum BitmapFlip: i32 {
			None (BitmapFlip_None) = 0, Horizontal (BitmapFlip_Horizontal) = 1, Vertical (BitmapFlip_Vertical) = 2,
		}}
		RT_ENUM! { enum BitmapInterpolationMode: i32 {
			NearestNeighbor (BitmapInterpolationMode_NearestNeighbor) = 0, Linear (BitmapInterpolationMode_Linear) = 1, Cubic (BitmapInterpolationMode_Cubic) = 2, Fant (BitmapInterpolationMode_Fant) = 3,
		}}
		RT_CLASS!(PixelDataProvider: ::rt::gen::windows::graphics::imaging::IPixelDataProvider);
		DEFINE_IID!(IID_IPixelDataProvider, 3716357925, 6236, 17813, 159, 185, 204, 190, 110, 193, 138, 111);
		RT_INTERFACE!{interface IPixelDataProvider(IPixelDataProviderVtbl): IInspectable(IInspectableVtbl) [IID_IPixelDataProvider] {
			fn DetachPixelData(&mut self, out: *mut *mut u8) -> ::w::HRESULT
		}}
		RT_CLASS!(BitmapPropertiesView: ::rt::gen::windows::graphics::imaging::IBitmapPropertiesView);
		DEFINE_IID!(IID_IBitmapPropertiesView, 2114971770, 14960, 18680, 156, 85, 25, 108, 245, 165, 69, 245);
		RT_INTERFACE!{interface IBitmapPropertiesView(IBitmapPropertiesViewVtbl): IInspectable(IInspectableVtbl) [IID_IBitmapPropertiesView] {
			fn GetPropertiesAsync(&mut self, propertiesToRetrieve: *mut ::rt::gen::windows::foundation::collections::IIterable<&str>, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::graphics::imaging::BitmapPropertySet>) -> ::w::HRESULT
		}}
		RT_CLASS!(BitmapPropertySet: ::rt::gen::windows::foundation::collections::IMap<&'static str, &'static ::rt::gen::windows::graphics::imaging::BitmapTypedValue>);
		RT_CLASS!(BitmapTypedValue: ::rt::gen::windows::graphics::imaging::IBitmapTypedValue);
		DEFINE_IID!(IID_IBitmapTypedValue, 3447735465, 9283, 16384, 176, 205, 121, 49, 108, 86, 245, 137);
		RT_INTERFACE!{interface IBitmapTypedValue(IBitmapTypedValueVtbl): IInspectable(IInspectableVtbl) [IID_IBitmapTypedValue] {
			fn get_Value(&mut self, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn get_Type(&mut self, out: *mut ::rt::gen::windows::foundation::PropertyType) -> ::w::HRESULT
		}}
		RT_CLASS!(ImageStream: ::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType);
		RT_CLASS!(SoftwareBitmap: ::rt::gen::windows::graphics::imaging::ISoftwareBitmap);
		DEFINE_IID!(IID_ISoftwareBitmap, 1755186952, 32495, 18495, 150, 63, 218, 147, 136, 24, 224, 115);
		RT_INTERFACE!{interface ISoftwareBitmap(ISoftwareBitmapVtbl): IInspectable(IInspectableVtbl) [IID_ISoftwareBitmap] {
			fn get_BitmapPixelFormat(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapPixelFormat) -> ::w::HRESULT,
			fn get_BitmapAlphaMode(&mut self, out: *mut ::rt::gen::windows::graphics::imaging::BitmapAlphaMode) -> ::w::HRESULT,
			fn get_PixelWidth(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_PixelHeight(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_IsReadOnly(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn put_DpiX(&mut self, value: f64) -> ::w::HRESULT,
			fn get_DpiX(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn put_DpiY(&mut self, value: f64) -> ::w::HRESULT,
			fn get_DpiY(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn LockBuffer(&mut self, mode: ::rt::gen::windows::graphics::imaging::BitmapBufferAccessMode, out: *mut *mut ::rt::gen::windows::graphics::imaging::BitmapBuffer) -> ::w::HRESULT,
			fn CopyTo(&mut self, bitmap: *mut ::rt::gen::windows::graphics::imaging::SoftwareBitmap) -> ::w::HRESULT,
			fn CopyFromBuffer(&mut self, buffer: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn CopyToBuffer(&mut self, buffer: *mut ::rt::gen::windows::storage::streams::IBuffer) -> ::w::HRESULT,
			fn GetReadOnlyView(&mut self, out: *mut *mut ::rt::gen::windows::graphics::imaging::SoftwareBitmap) -> ::w::HRESULT
		}}
		RT_CLASS!(BitmapBuffer: ::rt::gen::windows::graphics::imaging::IBitmapBuffer);
		DEFINE_IID!(IID_IBitmapBuffer, 2772305092, 14748, 17292, 178, 143, 166, 58, 107, 131, 209, 161);
		RT_INTERFACE!{interface IBitmapBuffer(IBitmapBufferVtbl): IInspectable(IInspectableVtbl) [IID_IBitmapBuffer] {
			fn GetPlaneCount(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn GetPlaneDescription(&mut self, index: i32, out: *mut ::rt::gen::windows::graphics::imaging::BitmapPlaneDescription) -> ::w::HRESULT
		}}
		RT_STRUCT! { struct BitmapPlaneDescription {
			StartIndex: i32, Width: i32, Height: i32, Stride: i32,
		}}
		RT_ENUM! { enum BitmapBufferAccessMode: i32 {
			Read (BitmapBufferAccessMode_Read) = 0, ReadWrite (BitmapBufferAccessMode_ReadWrite) = 1, Write (BitmapBufferAccessMode_Write) = 2,
		}}
} // Windows.Graphics.Imaging
pub mod display { // Windows.Graphics.Display
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum DisplayOrientations: u32 {
			None (DisplayOrientations_None) = 0, Landscape (DisplayOrientations_Landscape) = 1, Portrait (DisplayOrientations_Portrait) = 2, LandscapeFlipped (DisplayOrientations_LandscapeFlipped) = 4, PortraitFlipped (DisplayOrientations_PortraitFlipped) = 8,
		}}
} // Windows.Graphics.Display
pub mod directx { // Windows.Graphics.DirectX
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum DirectXPixelFormat: i32 {
			Unknown (DirectXPixelFormat_Unknown) = 0, R32G32B32A32Typeless (DirectXPixelFormat_R32G32B32A32Typeless) = 1, R32G32B32A32Float (DirectXPixelFormat_R32G32B32A32Float) = 2, R32G32B32A32UInt (DirectXPixelFormat_R32G32B32A32UInt) = 3, R32G32B32A32Int (DirectXPixelFormat_R32G32B32A32Int) = 4, R32G32B32Typeless (DirectXPixelFormat_R32G32B32Typeless) = 5, R32G32B32Float (DirectXPixelFormat_R32G32B32Float) = 6, R32G32B32UInt (DirectXPixelFormat_R32G32B32UInt) = 7, R32G32B32Int (DirectXPixelFormat_R32G32B32Int) = 8, R16G16B16A16Typeless (DirectXPixelFormat_R16G16B16A16Typeless) = 9, R16G16B16A16Float (DirectXPixelFormat_R16G16B16A16Float) = 10, R16G16B16A16UIntNormalized (DirectXPixelFormat_R16G16B16A16UIntNormalized) = 11, R16G16B16A16UInt (DirectXPixelFormat_R16G16B16A16UInt) = 12, R16G16B16A16IntNormalized (DirectXPixelFormat_R16G16B16A16IntNormalized) = 13, R16G16B16A16Int (DirectXPixelFormat_R16G16B16A16Int) = 14, R32G32Typeless (DirectXPixelFormat_R32G32Typeless) = 15, R32G32Float (DirectXPixelFormat_R32G32Float) = 16, R32G32UInt (DirectXPixelFormat_R32G32UInt) = 17, R32G32Int (DirectXPixelFormat_R32G32Int) = 18, R32G8X24Typeless (DirectXPixelFormat_R32G8X24Typeless) = 19, D32FloatS8X24UInt (DirectXPixelFormat_D32FloatS8X24UInt) = 20, R32FloatX8X24Typeless (DirectXPixelFormat_R32FloatX8X24Typeless) = 21, X32TypelessG8X24UInt (DirectXPixelFormat_X32TypelessG8X24UInt) = 22, R10G10B10A2Typeless (DirectXPixelFormat_R10G10B10A2Typeless) = 23, R10G10B10A2UIntNormalized (DirectXPixelFormat_R10G10B10A2UIntNormalized) = 24, R10G10B10A2UInt (DirectXPixelFormat_R10G10B10A2UInt) = 25, R11G11B10Float (DirectXPixelFormat_R11G11B10Float) = 26, R8G8B8A8Typeless (DirectXPixelFormat_R8G8B8A8Typeless) = 27, R8G8B8A8UIntNormalized (DirectXPixelFormat_R8G8B8A8UIntNormalized) = 28, R8G8B8A8UIntNormalizedSrgb (DirectXPixelFormat_R8G8B8A8UIntNormalizedSrgb) = 29, R8G8B8A8UInt (DirectXPixelFormat_R8G8B8A8UInt) = 30, R8G8B8A8IntNormalized (DirectXPixelFormat_R8G8B8A8IntNormalized) = 31, R8G8B8A8Int (DirectXPixelFormat_R8G8B8A8Int) = 32, R16G16Typeless (DirectXPixelFormat_R16G16Typeless) = 33, R16G16Float (DirectXPixelFormat_R16G16Float) = 34, R16G16UIntNormalized (DirectXPixelFormat_R16G16UIntNormalized) = 35, R16G16UInt (DirectXPixelFormat_R16G16UInt) = 36, R16G16IntNormalized (DirectXPixelFormat_R16G16IntNormalized) = 37, R16G16Int (DirectXPixelFormat_R16G16Int) = 38, R32Typeless (DirectXPixelFormat_R32Typeless) = 39, D32Float (DirectXPixelFormat_D32Float) = 40, R32Float (DirectXPixelFormat_R32Float) = 41, R32UInt (DirectXPixelFormat_R32UInt) = 42, R32Int (DirectXPixelFormat_R32Int) = 43, R24G8Typeless (DirectXPixelFormat_R24G8Typeless) = 44, D24UIntNormalizedS8UInt (DirectXPixelFormat_D24UIntNormalizedS8UInt) = 45, R24UIntNormalizedX8Typeless (DirectXPixelFormat_R24UIntNormalizedX8Typeless) = 46, X24TypelessG8UInt (DirectXPixelFormat_X24TypelessG8UInt) = 47, R8G8Typeless (DirectXPixelFormat_R8G8Typeless) = 48, R8G8UIntNormalized (DirectXPixelFormat_R8G8UIntNormalized) = 49, R8G8UInt (DirectXPixelFormat_R8G8UInt) = 50, R8G8IntNormalized (DirectXPixelFormat_R8G8IntNormalized) = 51, R8G8Int (DirectXPixelFormat_R8G8Int) = 52, R16Typeless (DirectXPixelFormat_R16Typeless) = 53, R16Float (DirectXPixelFormat_R16Float) = 54, D16UIntNormalized (DirectXPixelFormat_D16UIntNormalized) = 55, R16UIntNormalized (DirectXPixelFormat_R16UIntNormalized) = 56, R16UInt (DirectXPixelFormat_R16UInt) = 57, R16IntNormalized (DirectXPixelFormat_R16IntNormalized) = 58, R16Int (DirectXPixelFormat_R16Int) = 59, R8Typeless (DirectXPixelFormat_R8Typeless) = 60, R8UIntNormalized (DirectXPixelFormat_R8UIntNormalized) = 61, R8UInt (DirectXPixelFormat_R8UInt) = 62, R8IntNormalized (DirectXPixelFormat_R8IntNormalized) = 63, R8Int (DirectXPixelFormat_R8Int) = 64, A8UIntNormalized (DirectXPixelFormat_A8UIntNormalized) = 65, R1UIntNormalized (DirectXPixelFormat_R1UIntNormalized) = 66, R9G9B9E5SharedExponent (DirectXPixelFormat_R9G9B9E5SharedExponent) = 67, R8G8B8G8UIntNormalized (DirectXPixelFormat_R8G8B8G8UIntNormalized) = 68, G8R8G8B8UIntNormalized (DirectXPixelFormat_G8R8G8B8UIntNormalized) = 69, BC1Typeless (DirectXPixelFormat_BC1Typeless) = 70, BC1UIntNormalized (DirectXPixelFormat_BC1UIntNormalized) = 71, BC1UIntNormalizedSrgb (DirectXPixelFormat_BC1UIntNormalizedSrgb) = 72, BC2Typeless (DirectXPixelFormat_BC2Typeless) = 73, BC2UIntNormalized (DirectXPixelFormat_BC2UIntNormalized) = 74, BC2UIntNormalizedSrgb (DirectXPixelFormat_BC2UIntNormalizedSrgb) = 75, BC3Typeless (DirectXPixelFormat_BC3Typeless) = 76, BC3UIntNormalized (DirectXPixelFormat_BC3UIntNormalized) = 77, BC3UIntNormalizedSrgb (DirectXPixelFormat_BC3UIntNormalizedSrgb) = 78, BC4Typeless (DirectXPixelFormat_BC4Typeless) = 79, BC4UIntNormalized (DirectXPixelFormat_BC4UIntNormalized) = 80, BC4IntNormalized (DirectXPixelFormat_BC4IntNormalized) = 81, BC5Typeless (DirectXPixelFormat_BC5Typeless) = 82, BC5UIntNormalized (DirectXPixelFormat_BC5UIntNormalized) = 83, BC5IntNormalized (DirectXPixelFormat_BC5IntNormalized) = 84, B5G6R5UIntNormalized (DirectXPixelFormat_B5G6R5UIntNormalized) = 85, B5G5R5A1UIntNormalized (DirectXPixelFormat_B5G5R5A1UIntNormalized) = 86, B8G8R8A8UIntNormalized (DirectXPixelFormat_B8G8R8A8UIntNormalized) = 87, B8G8R8X8UIntNormalized (DirectXPixelFormat_B8G8R8X8UIntNormalized) = 88, R10G10B10XRBiasA2UIntNormalized (DirectXPixelFormat_R10G10B10XRBiasA2UIntNormalized) = 89, B8G8R8A8Typeless (DirectXPixelFormat_B8G8R8A8Typeless) = 90, B8G8R8A8UIntNormalizedSrgb (DirectXPixelFormat_B8G8R8A8UIntNormalizedSrgb) = 91, B8G8R8X8Typeless (DirectXPixelFormat_B8G8R8X8Typeless) = 92, B8G8R8X8UIntNormalizedSrgb (DirectXPixelFormat_B8G8R8X8UIntNormalizedSrgb) = 93, BC6HTypeless (DirectXPixelFormat_BC6HTypeless) = 94, BC6H16UnsignedFloat (DirectXPixelFormat_BC6H16UnsignedFloat) = 95, BC6H16Float (DirectXPixelFormat_BC6H16Float) = 96, BC7Typeless (DirectXPixelFormat_BC7Typeless) = 97, BC7UIntNormalized (DirectXPixelFormat_BC7UIntNormalized) = 98, BC7UIntNormalizedSrgb (DirectXPixelFormat_BC7UIntNormalizedSrgb) = 99, Ayuv (DirectXPixelFormat_Ayuv) = 100, Y410 (DirectXPixelFormat_Y410) = 101, Y416 (DirectXPixelFormat_Y416) = 102, NV12 (DirectXPixelFormat_NV12) = 103, P010 (DirectXPixelFormat_P010) = 104, P016 (DirectXPixelFormat_P016) = 105, Opaque420 (DirectXPixelFormat_Opaque420) = 106, Yuy2 (DirectXPixelFormat_Yuy2) = 107, Y210 (DirectXPixelFormat_Y210) = 108, Y216 (DirectXPixelFormat_Y216) = 109, NV11 (DirectXPixelFormat_NV11) = 110, AI44 (DirectXPixelFormat_AI44) = 111, IA44 (DirectXPixelFormat_IA44) = 112, P8 (DirectXPixelFormat_P8) = 113, A8P8 (DirectXPixelFormat_A8P8) = 114, B4G4R4A4UIntNormalized (DirectXPixelFormat_B4G4R4A4UIntNormalized) = 115, P208 (DirectXPixelFormat_P208) = 130, V208 (DirectXPixelFormat_V208) = 131, V408 (DirectXPixelFormat_V408) = 132,
		}}
pub mod direct3d11 { // Windows.Graphics.DirectX.Direct3D11
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_IDirect3DSurface, 200581446, 5057, 18068, 190, 227, 122, 191, 21, 234, 245, 134);
		RT_INTERFACE!{interface IDirect3DSurface(IDirect3DSurfaceVtbl): IInspectable(IInspectableVtbl) [IID_IDirect3DSurface] {
			fn get_Description(&mut self, out: *mut ::rt::gen::windows::graphics::directx::direct3d11::Direct3DSurfaceDescription) -> ::w::HRESULT
		}}
		RT_STRUCT! { struct Direct3DSurfaceDescription {
			Width: i32, Height: i32, Format: ::rt::gen::windows::graphics::directx::DirectXPixelFormat, MultisampleDescription: ::rt::gen::windows::graphics::directx::direct3d11::Direct3DMultisampleDescription,
		}}
		RT_STRUCT! { struct Direct3DMultisampleDescription {
			Count: i32, Quality: i32,
		}}
} // Windows.Graphics.DirectX.Direct3D11
} // Windows.Graphics.DirectX
pub mod printing { // Windows.Graphics.Printing
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum PrintOrientation: i32 {
			Default (PrintOrientation_Default) = 0, NotAvailable (PrintOrientation_NotAvailable) = 1, PrinterCustom (PrintOrientation_PrinterCustom) = 2, Portrait (PrintOrientation_Portrait) = 3, PortraitFlipped (PrintOrientation_PortraitFlipped) = 4, Landscape (PrintOrientation_Landscape) = 5, LandscapeFlipped (PrintOrientation_LandscapeFlipped) = 6,
		}}
		RT_ENUM! { enum PrintMediaSize: i32 {
			Default (PrintMediaSize_Default) = 0, NotAvailable (PrintMediaSize_NotAvailable) = 1, PrinterCustom (PrintMediaSize_PrinterCustom) = 2, BusinessCard (PrintMediaSize_BusinessCard) = 3, CreditCard (PrintMediaSize_CreditCard) = 4, IsoA0 (PrintMediaSize_IsoA0) = 5, IsoA1 (PrintMediaSize_IsoA1) = 6, IsoA10 (PrintMediaSize_IsoA10) = 7, IsoA2 (PrintMediaSize_IsoA2) = 8, IsoA3 (PrintMediaSize_IsoA3) = 9, IsoA3Extra (PrintMediaSize_IsoA3Extra) = 10, IsoA3Rotated (PrintMediaSize_IsoA3Rotated) = 11, IsoA4 (PrintMediaSize_IsoA4) = 12, IsoA4Extra (PrintMediaSize_IsoA4Extra) = 13, IsoA4Rotated (PrintMediaSize_IsoA4Rotated) = 14, IsoA5 (PrintMediaSize_IsoA5) = 15, IsoA5Extra (PrintMediaSize_IsoA5Extra) = 16, IsoA5Rotated (PrintMediaSize_IsoA5Rotated) = 17, IsoA6 (PrintMediaSize_IsoA6) = 18, IsoA6Rotated (PrintMediaSize_IsoA6Rotated) = 19, IsoA7 (PrintMediaSize_IsoA7) = 20, IsoA8 (PrintMediaSize_IsoA8) = 21, IsoA9 (PrintMediaSize_IsoA9) = 22, IsoB0 (PrintMediaSize_IsoB0) = 23, IsoB1 (PrintMediaSize_IsoB1) = 24, IsoB10 (PrintMediaSize_IsoB10) = 25, IsoB2 (PrintMediaSize_IsoB2) = 26, IsoB3 (PrintMediaSize_IsoB3) = 27, IsoB4 (PrintMediaSize_IsoB4) = 28, IsoB4Envelope (PrintMediaSize_IsoB4Envelope) = 29, IsoB5Envelope (PrintMediaSize_IsoB5Envelope) = 30, IsoB5Extra (PrintMediaSize_IsoB5Extra) = 31, IsoB7 (PrintMediaSize_IsoB7) = 32, IsoB8 (PrintMediaSize_IsoB8) = 33, IsoB9 (PrintMediaSize_IsoB9) = 34, IsoC0 (PrintMediaSize_IsoC0) = 35, IsoC1 (PrintMediaSize_IsoC1) = 36, IsoC10 (PrintMediaSize_IsoC10) = 37, IsoC2 (PrintMediaSize_IsoC2) = 38, IsoC3 (PrintMediaSize_IsoC3) = 39, IsoC3Envelope (PrintMediaSize_IsoC3Envelope) = 40, IsoC4 (PrintMediaSize_IsoC4) = 41, IsoC4Envelope (PrintMediaSize_IsoC4Envelope) = 42, IsoC5 (PrintMediaSize_IsoC5) = 43, IsoC5Envelope (PrintMediaSize_IsoC5Envelope) = 44, IsoC6 (PrintMediaSize_IsoC6) = 45, IsoC6C5Envelope (PrintMediaSize_IsoC6C5Envelope) = 46, IsoC6Envelope (PrintMediaSize_IsoC6Envelope) = 47, IsoC7 (PrintMediaSize_IsoC7) = 48, IsoC8 (PrintMediaSize_IsoC8) = 49, IsoC9 (PrintMediaSize_IsoC9) = 50, IsoDLEnvelope (PrintMediaSize_IsoDLEnvelope) = 51, IsoDLEnvelopeRotated (PrintMediaSize_IsoDLEnvelopeRotated) = 52, IsoSRA3 (PrintMediaSize_IsoSRA3) = 53, Japan2LPhoto (PrintMediaSize_Japan2LPhoto) = 54, JapanChou3Envelope (PrintMediaSize_JapanChou3Envelope) = 55, JapanChou3EnvelopeRotated (PrintMediaSize_JapanChou3EnvelopeRotated) = 56, JapanChou4Envelope (PrintMediaSize_JapanChou4Envelope) = 57, JapanChou4EnvelopeRotated (PrintMediaSize_JapanChou4EnvelopeRotated) = 58, JapanDoubleHagakiPostcard (PrintMediaSize_JapanDoubleHagakiPostcard) = 59, JapanDoubleHagakiPostcardRotated (PrintMediaSize_JapanDoubleHagakiPostcardRotated) = 60, JapanHagakiPostcard (PrintMediaSize_JapanHagakiPostcard) = 61, JapanHagakiPostcardRotated (PrintMediaSize_JapanHagakiPostcardRotated) = 62, JapanKaku2Envelope (PrintMediaSize_JapanKaku2Envelope) = 63, JapanKaku2EnvelopeRotated (PrintMediaSize_JapanKaku2EnvelopeRotated) = 64, JapanKaku3Envelope (PrintMediaSize_JapanKaku3Envelope) = 65, JapanKaku3EnvelopeRotated (PrintMediaSize_JapanKaku3EnvelopeRotated) = 66, JapanLPhoto (PrintMediaSize_JapanLPhoto) = 67, JapanQuadrupleHagakiPostcard (PrintMediaSize_JapanQuadrupleHagakiPostcard) = 68, JapanYou1Envelope (PrintMediaSize_JapanYou1Envelope) = 69, JapanYou2Envelope (PrintMediaSize_JapanYou2Envelope) = 70, JapanYou3Envelope (PrintMediaSize_JapanYou3Envelope) = 71, JapanYou4Envelope (PrintMediaSize_JapanYou4Envelope) = 72, JapanYou4EnvelopeRotated (PrintMediaSize_JapanYou4EnvelopeRotated) = 73, JapanYou6Envelope (PrintMediaSize_JapanYou6Envelope) = 74, JapanYou6EnvelopeRotated (PrintMediaSize_JapanYou6EnvelopeRotated) = 75, JisB0 (PrintMediaSize_JisB0) = 76, JisB1 (PrintMediaSize_JisB1) = 77, JisB10 (PrintMediaSize_JisB10) = 78, JisB2 (PrintMediaSize_JisB2) = 79, JisB3 (PrintMediaSize_JisB3) = 80, JisB4 (PrintMediaSize_JisB4) = 81, JisB4Rotated (PrintMediaSize_JisB4Rotated) = 82, JisB5 (PrintMediaSize_JisB5) = 83, JisB5Rotated (PrintMediaSize_JisB5Rotated) = 84, JisB6 (PrintMediaSize_JisB6) = 85, JisB6Rotated (PrintMediaSize_JisB6Rotated) = 86, JisB7 (PrintMediaSize_JisB7) = 87, JisB8 (PrintMediaSize_JisB8) = 88, JisB9 (PrintMediaSize_JisB9) = 89, NorthAmerica10x11 (PrintMediaSize_NorthAmerica10x11) = 90, NorthAmerica10x12 (PrintMediaSize_NorthAmerica10x12) = 91, NorthAmerica10x14 (PrintMediaSize_NorthAmerica10x14) = 92, NorthAmerica11x17 (PrintMediaSize_NorthAmerica11x17) = 93, NorthAmerica14x17 (PrintMediaSize_NorthAmerica14x17) = 94, NorthAmerica4x6 (PrintMediaSize_NorthAmerica4x6) = 95, NorthAmerica4x8 (PrintMediaSize_NorthAmerica4x8) = 96, NorthAmerica5x7 (PrintMediaSize_NorthAmerica5x7) = 97, NorthAmerica8x10 (PrintMediaSize_NorthAmerica8x10) = 98, NorthAmerica9x11 (PrintMediaSize_NorthAmerica9x11) = 99, NorthAmericaArchitectureASheet (PrintMediaSize_NorthAmericaArchitectureASheet) = 100, NorthAmericaArchitectureBSheet (PrintMediaSize_NorthAmericaArchitectureBSheet) = 101, NorthAmericaArchitectureCSheet (PrintMediaSize_NorthAmericaArchitectureCSheet) = 102, NorthAmericaArchitectureDSheet (PrintMediaSize_NorthAmericaArchitectureDSheet) = 103, NorthAmericaArchitectureESheet (PrintMediaSize_NorthAmericaArchitectureESheet) = 104, NorthAmericaCSheet (PrintMediaSize_NorthAmericaCSheet) = 105, NorthAmericaDSheet (PrintMediaSize_NorthAmericaDSheet) = 106, NorthAmericaESheet (PrintMediaSize_NorthAmericaESheet) = 107, NorthAmericaExecutive (PrintMediaSize_NorthAmericaExecutive) = 108, NorthAmericaGermanLegalFanfold (PrintMediaSize_NorthAmericaGermanLegalFanfold) = 109, NorthAmericaGermanStandardFanfold (PrintMediaSize_NorthAmericaGermanStandardFanfold) = 110, NorthAmericaLegal (PrintMediaSize_NorthAmericaLegal) = 111, NorthAmericaLegalExtra (PrintMediaSize_NorthAmericaLegalExtra) = 112, NorthAmericaLetter (PrintMediaSize_NorthAmericaLetter) = 113, NorthAmericaLetterExtra (PrintMediaSize_NorthAmericaLetterExtra) = 114, NorthAmericaLetterPlus (PrintMediaSize_NorthAmericaLetterPlus) = 115, NorthAmericaLetterRotated (PrintMediaSize_NorthAmericaLetterRotated) = 116, NorthAmericaMonarchEnvelope (PrintMediaSize_NorthAmericaMonarchEnvelope) = 117, NorthAmericaNote (PrintMediaSize_NorthAmericaNote) = 118, NorthAmericaNumber10Envelope (PrintMediaSize_NorthAmericaNumber10Envelope) = 119, NorthAmericaNumber10EnvelopeRotated (PrintMediaSize_NorthAmericaNumber10EnvelopeRotated) = 120, NorthAmericaNumber11Envelope (PrintMediaSize_NorthAmericaNumber11Envelope) = 121, NorthAmericaNumber12Envelope (PrintMediaSize_NorthAmericaNumber12Envelope) = 122, NorthAmericaNumber14Envelope (PrintMediaSize_NorthAmericaNumber14Envelope) = 123, NorthAmericaNumber9Envelope (PrintMediaSize_NorthAmericaNumber9Envelope) = 124, NorthAmericaPersonalEnvelope (PrintMediaSize_NorthAmericaPersonalEnvelope) = 125, NorthAmericaQuarto (PrintMediaSize_NorthAmericaQuarto) = 126, NorthAmericaStatement (PrintMediaSize_NorthAmericaStatement) = 127, NorthAmericaSuperA (PrintMediaSize_NorthAmericaSuperA) = 128, NorthAmericaSuperB (PrintMediaSize_NorthAmericaSuperB) = 129, NorthAmericaTabloid (PrintMediaSize_NorthAmericaTabloid) = 130, NorthAmericaTabloidExtra (PrintMediaSize_NorthAmericaTabloidExtra) = 131, OtherMetricA3Plus (PrintMediaSize_OtherMetricA3Plus) = 132, OtherMetricA4Plus (PrintMediaSize_OtherMetricA4Plus) = 133, OtherMetricFolio (PrintMediaSize_OtherMetricFolio) = 134, OtherMetricInviteEnvelope (PrintMediaSize_OtherMetricInviteEnvelope) = 135, OtherMetricItalianEnvelope (PrintMediaSize_OtherMetricItalianEnvelope) = 136, Prc10Envelope (PrintMediaSize_Prc10Envelope) = 137, Prc10EnvelopeRotated (PrintMediaSize_Prc10EnvelopeRotated) = 138, Prc16K (PrintMediaSize_Prc16K) = 139, Prc16KRotated (PrintMediaSize_Prc16KRotated) = 140, Prc1Envelope (PrintMediaSize_Prc1Envelope) = 141, Prc1EnvelopeRotated (PrintMediaSize_Prc1EnvelopeRotated) = 142, Prc2Envelope (PrintMediaSize_Prc2Envelope) = 143, Prc2EnvelopeRotated (PrintMediaSize_Prc2EnvelopeRotated) = 144, Prc32K (PrintMediaSize_Prc32K) = 145, Prc32KBig (PrintMediaSize_Prc32KBig) = 146, Prc32KRotated (PrintMediaSize_Prc32KRotated) = 147, Prc3Envelope (PrintMediaSize_Prc3Envelope) = 148, Prc3EnvelopeRotated (PrintMediaSize_Prc3EnvelopeRotated) = 149, Prc4Envelope (PrintMediaSize_Prc4Envelope) = 150, Prc4EnvelopeRotated (PrintMediaSize_Prc4EnvelopeRotated) = 151, Prc5Envelope (PrintMediaSize_Prc5Envelope) = 152, Prc5EnvelopeRotated (PrintMediaSize_Prc5EnvelopeRotated) = 153, Prc6Envelope (PrintMediaSize_Prc6Envelope) = 154, Prc6EnvelopeRotated (PrintMediaSize_Prc6EnvelopeRotated) = 155, Prc7Envelope (PrintMediaSize_Prc7Envelope) = 156, Prc7EnvelopeRotated (PrintMediaSize_Prc7EnvelopeRotated) = 157, Prc8Envelope (PrintMediaSize_Prc8Envelope) = 158, Prc8EnvelopeRotated (PrintMediaSize_Prc8EnvelopeRotated) = 159, Prc9Envelope (PrintMediaSize_Prc9Envelope) = 160, Prc9EnvelopeRotated (PrintMediaSize_Prc9EnvelopeRotated) = 161, Roll04Inch (PrintMediaSize_Roll04Inch) = 162, Roll06Inch (PrintMediaSize_Roll06Inch) = 163, Roll08Inch (PrintMediaSize_Roll08Inch) = 164, Roll12Inch (PrintMediaSize_Roll12Inch) = 165, Roll15Inch (PrintMediaSize_Roll15Inch) = 166, Roll18Inch (PrintMediaSize_Roll18Inch) = 167, Roll22Inch (PrintMediaSize_Roll22Inch) = 168, Roll24Inch (PrintMediaSize_Roll24Inch) = 169, Roll30Inch (PrintMediaSize_Roll30Inch) = 170, Roll36Inch (PrintMediaSize_Roll36Inch) = 171, Roll54Inch (PrintMediaSize_Roll54Inch) = 172,
		}}
} // Windows.Graphics.Printing
} // Windows.Graphics
pub mod media { // Windows.Media
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(VideoFrame: ::rt::gen::windows::media::IVideoFrame);
		DEFINE_IID!(IID_IVideoFrame, 213935653, 37116, 19602, 189, 149, 125, 237, 33, 129, 157, 28);
		RT_INTERFACE!{interface IVideoFrame(IVideoFrameVtbl): IInspectable(IInspectableVtbl) [IID_IVideoFrame] {
			fn get_SoftwareBitmap(&mut self, out: *mut *mut ::rt::gen::windows::graphics::imaging::SoftwareBitmap) -> ::w::HRESULT,
			fn CopyToAsync(&mut self, frame: *mut ::rt::gen::windows::media::VideoFrame, out: *mut *mut ::rt::gen::windows::foundation::IAsyncAction) -> ::w::HRESULT,
			fn get_Direct3DSurface(&mut self, out: *mut *mut ::rt::gen::windows::graphics::directx::direct3d11::IDirect3DSurface) -> ::w::HRESULT
		}}
pub mod devices { // Windows.Media.Devices
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
pub mod core { // Windows.Media.Devices.Core
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_CLASS!(CameraIntrinsics: ::rt::gen::windows::media::devices::core::ICameraIntrinsics);
		DEFINE_IID!(IID_ICameraIntrinsics, 178711858, 25993, 18906, 175, 222, 89, 66, 112, 202, 10, 172);
		RT_INTERFACE!{interface ICameraIntrinsics(ICameraIntrinsicsVtbl): IInspectable(IInspectableVtbl) [IID_ICameraIntrinsics] {
			fn get_FocalLength(&mut self, out: *mut ::rt::gen::windows::foundation::numerics::Vector2) -> ::w::HRESULT,
			fn get_PrincipalPoint(&mut self, out: *mut ::rt::gen::windows::foundation::numerics::Vector2) -> ::w::HRESULT,
			fn get_RadialDistortion(&mut self, out: *mut ::rt::gen::windows::foundation::numerics::Vector3) -> ::w::HRESULT,
			fn get_TangentialDistortion(&mut self, out: *mut ::rt::gen::windows::foundation::numerics::Vector2) -> ::w::HRESULT,
			fn get_ImageWidth(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_ImageHeight(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn ProjectOntoFrame(&mut self, coordinate: ::rt::gen::windows::foundation::numerics::Vector3, out: *mut ::rt::gen::windows::foundation::Point) -> ::w::HRESULT,
			fn UnprojectAtUnitDepth(&mut self, pixelCoordinate: ::rt::gen::windows::foundation::Point, out: *mut ::rt::gen::windows::foundation::numerics::Vector2) -> ::w::HRESULT,
			fn ProjectManyOntoFrame(&mut self, coordinates: *mut ::rt::gen::windows::foundation::numerics::Vector3, results: *mut ::rt::gen::windows::foundation::Point) -> ::w::HRESULT,
			fn UnprojectPixelsAtUnitDepth(&mut self, pixelCoordinates: *mut ::rt::gen::windows::foundation::Point, results: *mut ::rt::gen::windows::foundation::numerics::Vector2) -> ::w::HRESULT
		}}
} // Windows.Media.Devices.Core
} // Windows.Media.Devices
} // Windows.Media
pub mod foundation { // Windows.Foundation
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		DEFINE_IID!(IID_IClosable, 819308585, 32676, 16422, 131, 187, 215, 91, 174, 78, 169, 158);
		RT_INTERFACE!{interface IClosable(IClosableVtbl): IInspectable(IInspectableVtbl) [IID_IClosable] {
			fn Close(&mut self) -> ::w::HRESULT
		}}
		RT_ENUM! { enum PropertyType: i32 {
			Empty (PropertyType_Empty) = 0, UInt8 (PropertyType_UInt8) = 1, Int16 (PropertyType_Int16) = 2, UInt16 (PropertyType_UInt16) = 3, Int32 (PropertyType_Int32) = 4, UInt32 (PropertyType_UInt32) = 5, Int64 (PropertyType_Int64) = 6, UInt64 (PropertyType_UInt64) = 7, Single (PropertyType_Single) = 8, Double (PropertyType_Double) = 9, Char16 (PropertyType_Char16) = 10, Boolean (PropertyType_Boolean) = 11, String (PropertyType_String) = 12, Inspectable (PropertyType_Inspectable) = 13, DateTime (PropertyType_DateTime) = 14, TimeSpan (PropertyType_TimeSpan) = 15, Guid (PropertyType_Guid) = 16, Point (PropertyType_Point) = 17, Size (PropertyType_Size) = 18, Rect (PropertyType_Rect) = 19, OtherType (PropertyType_OtherType) = 20, UInt8Array (PropertyType_UInt8Array) = 1025, Int16Array (PropertyType_Int16Array) = 1026, UInt16Array (PropertyType_UInt16Array) = 1027, Int32Array (PropertyType_Int32Array) = 1028, UInt32Array (PropertyType_UInt32Array) = 1029, Int64Array (PropertyType_Int64Array) = 1030, UInt64Array (PropertyType_UInt64Array) = 1031, SingleArray (PropertyType_SingleArray) = 1032, DoubleArray (PropertyType_DoubleArray) = 1033, Char16Array (PropertyType_Char16Array) = 1034, BooleanArray (PropertyType_BooleanArray) = 1035, StringArray (PropertyType_StringArray) = 1036, InspectableArray (PropertyType_InspectableArray) = 1037, DateTimeArray (PropertyType_DateTimeArray) = 1038, TimeSpanArray (PropertyType_TimeSpanArray) = 1039, GuidArray (PropertyType_GuidArray) = 1040, PointArray (PropertyType_PointArray) = 1041, SizeArray (PropertyType_SizeArray) = 1042, RectArray (PropertyType_RectArray) = 1043, OtherTypeArray (PropertyType_OtherTypeArray) = 1044,
		}}
		RT_STRUCT! { struct Point {
			X: f32, Y: f32,
		}}
		RT_STRUCT! { struct Size {
			Width: f32, Height: f32,
		}}
		RT_STRUCT! { struct Rect {
			X: f32, Y: f32, Width: f32, Height: f32,
		}}
		RT_STRUCT! { struct DateTime {
			UniversalTime: i64,
		}}
		RT_STRUCT! { struct TimeSpan {
			Duration: i64,
		}}
		DEFINE_IID!(IID_IPropertyValue, 1272349405, 30036, 16617, 154, 155, 130, 101, 78, 222, 126, 98);
		RT_INTERFACE!{interface IPropertyValue(IPropertyValueVtbl): IInspectable(IInspectableVtbl) [IID_IPropertyValue] {
			fn get_Type(&mut self, out: *mut ::rt::gen::windows::foundation::PropertyType) -> ::w::HRESULT,
			fn get_IsNumericScalar(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn GetUInt8(&mut self, out: *mut u8) -> ::w::HRESULT,
			fn GetInt16(&mut self, out: *mut i16) -> ::w::HRESULT,
			fn GetUInt16(&mut self, out: *mut u16) -> ::w::HRESULT,
			fn GetInt32(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn GetUInt32(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn GetInt64(&mut self, out: *mut i64) -> ::w::HRESULT,
			fn GetUInt64(&mut self, out: *mut u64) -> ::w::HRESULT,
			fn GetSingle(&mut self, out: *mut f32) -> ::w::HRESULT,
			fn GetDouble(&mut self, out: *mut f64) -> ::w::HRESULT,
			fn GetChar16(&mut self, out: *mut ::w::wchar_t) -> ::w::HRESULT,
			fn GetBoolean(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn GetString(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetGuid(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn GetDateTime(&mut self, out: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn GetTimeSpan(&mut self, out: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn GetPoint(&mut self, out: *mut ::rt::gen::windows::foundation::Point) -> ::w::HRESULT,
			fn GetSize(&mut self, out: *mut ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn GetRect(&mut self, out: *mut ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn GetUInt8Array(&mut self, value: *mut *mut u8) -> ::w::HRESULT,
			fn GetInt16Array(&mut self, value: *mut *mut i16) -> ::w::HRESULT,
			fn GetUInt16Array(&mut self, value: *mut *mut u16) -> ::w::HRESULT,
			fn GetInt32Array(&mut self, value: *mut *mut i32) -> ::w::HRESULT,
			fn GetUInt32Array(&mut self, value: *mut *mut u32) -> ::w::HRESULT,
			fn GetInt64Array(&mut self, value: *mut *mut i64) -> ::w::HRESULT,
			fn GetUInt64Array(&mut self, value: *mut *mut u64) -> ::w::HRESULT,
			fn GetSingleArray(&mut self, value: *mut *mut f32) -> ::w::HRESULT,
			fn GetDoubleArray(&mut self, value: *mut *mut f64) -> ::w::HRESULT,
			fn GetChar16Array(&mut self, value: *mut *mut ::w::wchar_t) -> ::w::HRESULT,
			fn GetBooleanArray(&mut self, value: *mut *mut ::w::BOOL) -> ::w::HRESULT,
			fn GetStringArray(&mut self, value: *mut *mut ::w::HSTRING) -> ::w::HRESULT,
			fn GetInspectableArray(&mut self, value: *mut *mut *mut IInspectable) -> ::w::HRESULT,
			fn GetGuidArray(&mut self, value: *mut *mut ::w::GUID) -> ::w::HRESULT,
			fn GetDateTimeArray(&mut self, value: *mut *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn GetTimeSpanArray(&mut self, value: *mut *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn GetPointArray(&mut self, value: *mut *mut ::rt::gen::windows::foundation::Point) -> ::w::HRESULT,
			fn GetSizeArray(&mut self, value: *mut *mut ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn GetRectArray(&mut self, value: *mut *mut ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPropertyValueStatics, 1654381512, 55602, 20468, 150, 185, 141, 150, 197, 193, 232, 88);
		RT_INTERFACE!{interface IPropertyValueStatics(IPropertyValueStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IPropertyValueStatics] {
			fn CreateEmpty(&mut self, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateUInt8(&mut self, value: u8, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateInt16(&mut self, value: i16, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateUInt16(&mut self, value: u16, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateInt32(&mut self, value: i32, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateUInt32(&mut self, value: u32, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateInt64(&mut self, value: i64, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateUInt64(&mut self, value: u64, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateSingle(&mut self, value: f32, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateDouble(&mut self, value: f64, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateChar16(&mut self, value: ::w::wchar_t, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateBoolean(&mut self, value: ::w::BOOL, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateString(&mut self, value: ::w::HSTRING, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateInspectable(&mut self, value: *mut IInspectable, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateGuid(&mut self, value: ::w::GUID, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateDateTime(&mut self, value: ::rt::gen::windows::foundation::DateTime, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateTimeSpan(&mut self, value: ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreatePoint(&mut self, value: ::rt::gen::windows::foundation::Point, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateSize(&mut self, value: ::rt::gen::windows::foundation::Size, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateRect(&mut self, value: ::rt::gen::windows::foundation::Rect, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateUInt8Array(&mut self, value: *mut u8, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateInt16Array(&mut self, value: *mut i16, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateUInt16Array(&mut self, value: *mut u16, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateInt32Array(&mut self, value: *mut i32, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateUInt32Array(&mut self, value: *mut u32, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateInt64Array(&mut self, value: *mut i64, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateUInt64Array(&mut self, value: *mut u64, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateSingleArray(&mut self, value: *mut f32, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateDoubleArray(&mut self, value: *mut f64, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateChar16Array(&mut self, value: *mut ::w::wchar_t, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateBooleanArray(&mut self, value: *mut ::w::BOOL, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateStringArray(&mut self, value: *mut ::w::HSTRING, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateInspectableArray(&mut self, value: *mut *mut IInspectable, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateGuidArray(&mut self, value: *mut ::w::GUID, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateDateTimeArray(&mut self, value: *mut ::rt::gen::windows::foundation::DateTime, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateTimeSpanArray(&mut self, value: *mut ::rt::gen::windows::foundation::TimeSpan, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreatePointArray(&mut self, value: *mut ::rt::gen::windows::foundation::Point, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateSizeArray(&mut self, value: *mut ::rt::gen::windows::foundation::Size, out: *mut *mut IInspectable) -> ::w::HRESULT,
			fn CreateRectArray(&mut self, value: *mut ::rt::gen::windows::foundation::Rect, out: *mut *mut IInspectable) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IStringable, 2520162132, 36534, 18672, 171, 206, 193, 178, 17, 230, 39, 195);
		RT_INTERFACE!{interface IStringable(IStringableVtbl): IInspectable(IInspectableVtbl) [IID_IStringable] {
			fn ToString(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_AsyncActionCompletedHandler, 2767019137, 30409, 16573, 139, 230, 177, 217, 15, 178, 10, 231);
		RT_DELEGATE!{delegate AsyncActionCompletedHandler(AsyncActionCompletedHandlerVtbl, AsyncActionCompletedHandlerImpl) [IID_AsyncActionCompletedHandler] {
			fn Invoke(&mut self, asyncInfo: *mut ::rt::gen::windows::foundation::IAsyncAction, asyncStatus: ::rt::gen::windows::foundation::AsyncStatus) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeferral, 3592853298, 15231, 18087, 180, 11, 79, 220, 162, 162, 198, 147);
		RT_INTERFACE!{interface IDeferral(IDeferralVtbl): IInspectable(IInspectableVtbl) [IID_IDeferral] {
			fn Complete(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_DeferralCompletedHandler, 3979518834, 62408, 20394, 156, 251, 71, 1, 72, 218, 56, 136);
		RT_DELEGATE!{delegate DeferralCompletedHandler(DeferralCompletedHandlerVtbl, DeferralCompletedHandlerImpl) [IID_DeferralCompletedHandler] {
			fn Invoke(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IDeferralFactory, 1705110725, 16309, 18482, 140, 169, 240, 97, 178, 129, 209, 58);
		RT_INTERFACE!{interface IDeferralFactory(IDeferralFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IDeferralFactory] {
			fn Create(&mut self, handler: *mut ::rt::gen::windows::foundation::DeferralCompletedHandler, out: *mut *mut ::rt::gen::windows::foundation::Deferral) -> ::w::HRESULT
		}}
		RT_CLASS!(Deferral: ::rt::gen::windows::foundation::IDeferral);
		RT_ENUM! { enum AsyncStatus: i32 {
			Canceled (AsyncStatus_Canceled) = 2, Completed (AsyncStatus_Completed) = 1, Error (AsyncStatus_Error) = 3, Started (AsyncStatus_Started) = 0,
		}}
		RT_STRUCT! { struct EventRegistrationToken {
			Value: i64,
		}}
		RT_STRUCT! { struct HResult {
			Value: i32,
		}}
		DEFINE_IID!(IID_IAsyncInfo, 54, 0, 0, 192, 0, 0, 0, 0, 0, 0, 70);
		RT_INTERFACE!{interface IAsyncInfo(IAsyncInfoVtbl): IInspectable(IInspectableVtbl) [IID_IAsyncInfo] {
			fn get_Id(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn get_Status(&mut self, out: *mut ::rt::gen::windows::foundation::AsyncStatus) -> ::w::HRESULT,
			fn get_ErrorCode(&mut self, out: *mut ::rt::gen::windows::foundation::HResult) -> ::w::HRESULT,
			fn Cancel(&mut self) -> ::w::HRESULT,
			fn Close(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAsyncAction, 1516535814, 33850, 19881, 134, 91, 157, 38, 229, 223, 173, 123);
		RT_INTERFACE!{interface IAsyncAction(IAsyncActionVtbl): IInspectable(IInspectableVtbl) [IID_IAsyncAction] {
			fn put_Completed(&mut self, handler: *mut ::rt::gen::windows::foundation::AsyncActionCompletedHandler) -> ::w::HRESULT,
			fn get_Completed(&mut self, out: *mut *mut ::rt::gen::windows::foundation::AsyncActionCompletedHandler) -> ::w::HRESULT,
			fn GetResults(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_AsyncOperationWithProgressCompletedHandler, 3898471453, 27303, 18147, 168, 226, 240, 9, 216, 64, 198, 39);
		RT_DELEGATE!{delegate AsyncOperationWithProgressCompletedHandler<TResult, TProgress>(AsyncOperationWithProgressCompletedHandlerVtbl, AsyncOperationWithProgressCompletedHandlerImpl) [IID_AsyncOperationWithProgressCompletedHandler] {
			fn Invoke(&mut self, asyncInfo: *mut ::rt::gen::windows::foundation::IAsyncOperationWithProgress<TResult, TProgress>, asyncStatus: ::rt::gen::windows::foundation::AsyncStatus) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAsyncOperationWithProgress, 3050321623, 58007, 18831, 186, 96, 2, 137, 231, 110, 35, 221);
		RT_INTERFACE!{interface IAsyncOperationWithProgress<TResult, TProgress>(IAsyncOperationWithProgressVtbl): IInspectable(IInspectableVtbl) [IID_IAsyncOperationWithProgress] {
			fn put_Progress(&mut self, handler: *mut ::rt::gen::windows::foundation::AsyncOperationProgressHandler<TResult, TProgress>) -> ::w::HRESULT,
			fn get_Progress(&mut self, out: *mut *mut ::rt::gen::windows::foundation::AsyncOperationProgressHandler<TResult, TProgress>) -> ::w::HRESULT,
			fn put_Completed(&mut self, handler: *mut ::rt::gen::windows::foundation::AsyncOperationWithProgressCompletedHandler<TResult, TProgress>) -> ::w::HRESULT,
			fn get_Completed(&mut self, out: *mut *mut ::rt::gen::windows::foundation::AsyncOperationWithProgressCompletedHandler<TResult, TProgress>) -> ::w::HRESULT,
			fn GetResults(&mut self, out: *mut TResult::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_AsyncOperationCompletedHandler, 4242337836, 58840, 17528, 145, 90, 77, 144, 183, 75, 131, 165);
		RT_DELEGATE!{delegate AsyncOperationCompletedHandler<TResult>(AsyncOperationCompletedHandlerVtbl, AsyncOperationCompletedHandlerImpl) [IID_AsyncOperationCompletedHandler] {
			fn Invoke(&mut self, asyncInfo: *mut ::rt::gen::windows::foundation::IAsyncOperation<TResult>, asyncStatus: ::rt::gen::windows::foundation::AsyncStatus) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAsyncOperation, 2680336571, 58438, 17634, 170, 97, 156, 171, 143, 99, 106, 242);
		RT_INTERFACE!{interface IAsyncOperation<TResult>(IAsyncOperationVtbl): IInspectable(IInspectableVtbl) [IID_IAsyncOperation] {
			fn put_Completed(&mut self, handler: *mut ::rt::gen::windows::foundation::AsyncOperationCompletedHandler<TResult>) -> ::w::HRESULT,
			fn get_Completed(&mut self, out: *mut *mut ::rt::gen::windows::foundation::AsyncOperationCompletedHandler<TResult>) -> ::w::HRESULT,
			fn GetResults(&mut self, out: *mut TResult::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_AsyncActionWithProgressCompletedHandler, 2617417617, 52356, 17661, 172, 38, 10, 108, 78, 85, 82, 129);
		RT_DELEGATE!{delegate AsyncActionWithProgressCompletedHandler<TProgress>(AsyncActionWithProgressCompletedHandlerVtbl, AsyncActionWithProgressCompletedHandlerImpl) [IID_AsyncActionWithProgressCompletedHandler] {
			fn Invoke(&mut self, asyncInfo: *mut ::rt::gen::windows::foundation::IAsyncActionWithProgress<TProgress>, asyncStatus: ::rt::gen::windows::foundation::AsyncStatus) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAsyncActionWithProgress, 527282776, 59395, 18593, 149, 70, 235, 115, 83, 57, 136, 132);
		RT_INTERFACE!{interface IAsyncActionWithProgress<TProgress>(IAsyncActionWithProgressVtbl): IInspectable(IInspectableVtbl) [IID_IAsyncActionWithProgress] {
			fn put_Progress(&mut self, handler: *mut ::rt::gen::windows::foundation::AsyncActionProgressHandler<TProgress>) -> ::w::HRESULT,
			fn get_Progress(&mut self, out: *mut *mut ::rt::gen::windows::foundation::AsyncActionProgressHandler<TProgress>) -> ::w::HRESULT,
			fn put_Completed(&mut self, handler: *mut ::rt::gen::windows::foundation::AsyncActionWithProgressCompletedHandler<TProgress>) -> ::w::HRESULT,
			fn get_Completed(&mut self, out: *mut *mut ::rt::gen::windows::foundation::AsyncActionWithProgressCompletedHandler<TProgress>) -> ::w::HRESULT,
			fn GetResults(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_AsyncOperationProgressHandler, 1432946946, 2731, 16922, 135, 120, 248, 206, 80, 38, 215, 88);
		RT_DELEGATE!{delegate AsyncOperationProgressHandler<TResult, TProgress>(AsyncOperationProgressHandlerVtbl, AsyncOperationProgressHandlerImpl) [IID_AsyncOperationProgressHandler] {
			fn Invoke(&mut self, asyncInfo: *mut ::rt::gen::windows::foundation::IAsyncOperationWithProgress<TResult, TProgress>, progressInfo: TProgress::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_AsyncActionProgressHandler, 1837385816, 3327, 17808, 174, 137, 149, 165, 165, 200, 180, 184);
		RT_DELEGATE!{delegate AsyncActionProgressHandler<TProgress>(AsyncActionProgressHandlerVtbl, AsyncActionProgressHandlerImpl) [IID_AsyncActionProgressHandler] {
			fn Invoke(&mut self, asyncInfo: *mut ::rt::gen::windows::foundation::IAsyncActionWithProgress<TProgress>, progressInfo: TProgress::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IReference, 1640068870, 11621, 4576, 154, 232, 212, 133, 100, 1, 84, 114);
		RT_INTERFACE!{interface IReference<T>(IReferenceVtbl): IInspectable(IInspectableVtbl) [IID_IReference] {
			fn get_Value(&mut self, out: *mut T::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IReferenceArray, 1640068871, 11621, 4576, 154, 232, 212, 133, 100, 1, 84, 114);
		RT_INTERFACE!{interface IReferenceArray<T>(IReferenceArrayVtbl): IInspectable(IInspectableVtbl) [IID_IReferenceArray] {
			fn get_Value(&mut self, out: *mut *mut T::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_TypedEventHandler, 2648818996, 27361, 4576, 132, 225, 24, 169, 5, 188, 197, 63);
		RT_DELEGATE!{delegate TypedEventHandler<TSender, TResult>(TypedEventHandlerVtbl, TypedEventHandlerImpl) [IID_TypedEventHandler] {
			fn Invoke(&mut self, sender: TSender::Abi, args: TResult::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_EventHandler, 2648818997, 27361, 4576, 132, 225, 24, 169, 5, 188, 197, 63);
		RT_DELEGATE!{delegate EventHandler<T>(EventHandlerVtbl, EventHandlerImpl) [IID_EventHandler] {
			fn Invoke(&mut self, sender: *mut IInspectable, args: T::Abi) -> ::w::HRESULT
		}}
		RT_STRUCT! { struct FoundationContract {
			
		}}
		DEFINE_IID!(IID_IUriRuntimeClass, 2654363223, 18610, 16736, 149, 111, 199, 56, 81, 32, 187, 252);
		RT_INTERFACE!{interface IUriRuntimeClass(IUriRuntimeClassVtbl): IInspectable(IInspectableVtbl) [IID_IUriRuntimeClass] {
			fn get_AbsoluteUri(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DisplayUri(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Domain(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Extension(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Fragment(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Host(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Password(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Path(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Query(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_QueryParsed(&mut self, out: *mut *mut ::rt::gen::windows::foundation::WwwFormUrlDecoder) -> ::w::HRESULT,
			fn get_RawUri(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_SchemeName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_UserName(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Port(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn get_Suspicious(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn Equals(&mut self, pUri: *mut ::rt::gen::windows::foundation::Uri, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn CombineUri(&mut self, relativeUri: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::Uri) -> ::w::HRESULT
		}}
		RT_CLASS!(WwwFormUrlDecoder: ::rt::gen::windows::foundation::IWwwFormUrlDecoderRuntimeClass);
		RT_CLASS!(Uri: ::rt::gen::windows::foundation::IUriRuntimeClass);
		DEFINE_IID!(IID_IUriRuntimeClassWithAbsoluteCanonicalUri, 1972213345, 8732, 18447, 163, 57, 80, 101, 102, 115, 244, 111);
		RT_INTERFACE!{interface IUriRuntimeClassWithAbsoluteCanonicalUri(IUriRuntimeClassWithAbsoluteCanonicalUriVtbl): IInspectable(IInspectableVtbl) [IID_IUriRuntimeClassWithAbsoluteCanonicalUri] {
			fn get_AbsoluteCanonicalUri(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_DisplayIri(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUriEscapeStatics, 3251909306, 51236, 17490, 167, 253, 81, 43, 195, 187, 233, 161);
		RT_INTERFACE!{interface IUriEscapeStatics(IUriEscapeStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IUriEscapeStatics] {
			fn UnescapeComponent(&mut self, toUnescape: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn EscapeComponent(&mut self, toEscape: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IUriRuntimeClassFactory, 1151957359, 29246, 20447, 162, 24, 3, 62, 117, 176, 192, 132);
		RT_INTERFACE!{interface IUriRuntimeClassFactory(IUriRuntimeClassFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IUriRuntimeClassFactory] {
			fn CreateUri(&mut self, uri: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::Uri) -> ::w::HRESULT,
			fn CreateWithRelativeUri(&mut self, baseUri: ::w::HSTRING, relativeUri: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::Uri) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWwwFormUrlDecoderEntry, 308180017, 63096, 20110, 182, 112, 32, 169, 176, 108, 81, 45);
		RT_INTERFACE!{interface IWwwFormUrlDecoderEntry(IWwwFormUrlDecoderEntryVtbl): IInspectable(IInspectableVtbl) [IID_IWwwFormUrlDecoderEntry] {
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Value(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWwwFormUrlDecoderRuntimeClass, 3562669137, 61989, 17730, 146, 150, 14, 29, 245, 210, 84, 223);
		RT_INTERFACE!{interface IWwwFormUrlDecoderRuntimeClass(IWwwFormUrlDecoderRuntimeClassVtbl): IInspectable(IInspectableVtbl) [IID_IWwwFormUrlDecoderRuntimeClass] {
			fn GetFirstValueByName(&mut self, name: ::w::HSTRING, out: *mut ::w::HSTRING) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IWwwFormUrlDecoderRuntimeClassFactory, 1535929149, 9390, 16821, 161, 191, 240, 195, 213, 68, 132, 91);
		RT_INTERFACE!{interface IWwwFormUrlDecoderRuntimeClassFactory(IWwwFormUrlDecoderRuntimeClassFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IWwwFormUrlDecoderRuntimeClassFactory] {
			fn CreateWwwFormUrlDecoder(&mut self, query: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::WwwFormUrlDecoder) -> ::w::HRESULT
		}}
		RT_CLASS!(WwwFormUrlDecoderEntry: ::rt::gen::windows::foundation::IWwwFormUrlDecoderEntry);
		DEFINE_IID!(IID_IGetActivationFactory, 1323011810, 38621, 18855, 148, 247, 70, 7, 221, 171, 142, 60);
		RT_INTERFACE!{interface IGetActivationFactory(IGetActivationFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IGetActivationFactory] {
			fn GetActivationFactory(&mut self, activatableClassId: ::w::HSTRING, out: *mut *mut IInspectable) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMemoryBufferReference, 4223982889, 9307, 4580, 175, 152, 104, 148, 35, 38, 12, 248);
		RT_INTERFACE!{interface IMemoryBufferReference(IMemoryBufferReferenceVtbl): IInspectable(IInspectableVtbl) [IID_IMemoryBufferReference] {
			fn get_Capacity(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn add_Closed(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::foundation::IMemoryBufferReference, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_Closed(&mut self, cookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMemoryBuffer, 4223982890, 9307, 4580, 175, 152, 104, 148, 35, 38, 12, 248);
		RT_INTERFACE!{interface IMemoryBuffer(IMemoryBufferVtbl): IInspectable(IInspectableVtbl) [IID_IMemoryBuffer] {
			fn CreateReference(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IMemoryBufferReference) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMemoryBufferFactory, 4223982891, 9307, 4580, 175, 152, 104, 148, 35, 38, 12, 248);
		RT_INTERFACE!{interface IMemoryBufferFactory(IMemoryBufferFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IMemoryBufferFactory] {
			fn Create(&mut self, capacity: u32, out: *mut *mut ::rt::gen::windows::foundation::MemoryBuffer) -> ::w::HRESULT
		}}
		RT_CLASS!(MemoryBuffer: ::rt::gen::windows::foundation::IMemoryBuffer);
		RT_STRUCT! { struct UniversalApiContract {
			
		}}
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::adc::AdcController>> => [0x1b0cddfb,0xd255,0x5a93,0xbc,0xb9,0xde,0x20,0x47,0xa3,0xe4,0xf3] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Adc_AdcController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::adc::AdcController>> => [0x7c4038c8,0xd920,0x53c7,0xa5,0xd6,0xa9,0x76,0x07,0x0d,0x76,0x37] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Adc_AdcController }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::adc::AdcController> => [0x69420262,0x35c9,0x583f,0xa4,0x0e,0xc2,0x69,0x45,0x62,0xc9,0xe2] as IID_IAsyncOperation_1_Windows_Devices_Adc_AdcController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::adc::AdcController> => [0xbaf66488,0x202f,0x5d51,0xb0,0x5e,0x18,0x60,0x6c,0x46,0xb8,0x08] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Adc_AdcController }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::gpio::provider::IGpioPinProvider, &'a ::rt::gen::windows::devices::gpio::provider::GpioPinProviderValueChangedEventArgs> => [0xaf259d89,0x9e01,0x529e,0xa8,0x79,0xc6,0x76,0x31,0x42,0xd1,0x60] as IID_TypedEventHandler_2_Windows_Devices_Gpio_Provider_IGpioPinProvider_Windows_Devices_Gpio_Provider_GpioPinProviderValueChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::gpio::GpioController>> => [0x5da3faf4,0x60a7,0x5a14,0x93,0x19,0x39,0x41,0xdf,0xb1,0x3f,0xed] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Gpio_GpioController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::gpio::GpioController>> => [0xee427f2e,0x7d37,0x558f,0x97,0x18,0x9c,0xbc,0xbf,0xf4,0x0c,0x94] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Gpio_GpioController }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::gpio::GpioController> => [0xed045917,0x96c7,0x5735,0xb4,0xbe,0xd7,0x96,0x19,0xd4,0x83,0x5e] as IID_IAsyncOperation_1_Windows_Devices_Gpio_GpioController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::gpio::GpioController> => [0x370167c0,0x0f7b,0x5e77,0x9b,0xae,0xd3,0x50,0x89,0xa3,0xdb,0x75] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Gpio_GpioController }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::i2c::provider::II2cControllerProvider>> => [0x5fe77838,0x1125,0x5b2c,0xa2,0x81,0xe0,0x6a,0x3d,0xfb,0xb7,0x6e] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_I2c_Provider_II2cControllerProvider }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::i2c::provider::II2cControllerProvider>> => [0x771e22ed,0xda9e,0x50be,0xb7,0x30,0xa3,0xba,0xda,0x6b,0xfb,0x25] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_I2c_Provider_II2cControllerProvider }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::i2c::I2cDevice> => [0x1e8a7cd8,0xe41b,0x5a41,0x82,0xb1,0x80,0x05,0x50,0x12,0xae,0x00] as IID_IAsyncOperation_1_Windows_Devices_I2c_I2cDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::i2c::I2cDevice> => [0x2df5bb6a,0x5e73,0x5ae3,0xa0,0xb2,0x22,0xe1,0xc9,0xd8,0xef,0x4d] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_I2c_I2cDevice }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::i2c::I2cController>> => [0x77f52ff7,0xaba0,0x54bb,0x89,0x1a,0x49,0x35,0x1a,0x83,0x8e,0x33] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_I2c_I2cController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::i2c::I2cController>> => [0x3b9d7cb1,0xae0b,0x56af,0x8e,0xd5,0x68,0x56,0xb1,0xe7,0xcd,0x5b] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_I2c_I2cController }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::i2c::I2cController> => [0xa4fb1dd4,0x80c9,0x5a61,0xae,0x8d,0xc8,0xa7,0xaf,0xc0,0x32,0x75] as IID_IAsyncOperation_1_Windows_Devices_I2c_I2cController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::i2c::I2cController> => [0x6ff64b72,0xa5aa,0x5986,0xb5,0x63,0x27,0x61,0x2a,0xfb,0x37,0x3c] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_I2c_I2cController }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::pwm::PwmController>> => [0xe4151e8d,0x4688,0x5023,0x9f,0x5d,0x00,0x8b,0xbd,0x90,0x48,0x91] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Pwm_PwmController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::pwm::PwmController>> => [0xe72bd078,0xce02,0x55ac,0xa7,0xb9,0xab,0xd0,0x12,0x48,0xd8,0x88] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Pwm_PwmController }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pwm::PwmController> => [0x0a288d41,0x1f20,0x5d16,0x85,0xdd,0x52,0x85,0x5b,0x11,0x56,0x9a] as IID_IAsyncOperation_1_Windows_Devices_Pwm_PwmController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pwm::PwmController> => [0x5fc68e9f,0xffff,0x5d53,0xba,0x21,0x9c,0x33,0xef,0x56,0xb2,0x40] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Pwm_PwmController }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::spi::provider::ISpiControllerProvider>> => [0xb3af3490,0xdede,0x59d1,0xb5,0x62,0x1f,0x6b,0xe7,0x1a,0xe1,0x39] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Spi_Provider_ISpiControllerProvider }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::spi::provider::ISpiControllerProvider>> => [0xe9e2ae03,0x42d6,0x5211,0xab,0x52,0x32,0x5e,0x72,0x2e,0x26,0x11] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Spi_Provider_ISpiControllerProvider }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::spi::SpiDevice> => [0xfeb8466a,0x878f,0x577b,0xbb,0xca,0x89,0x57,0x5c,0xfc,0x56,0xe4] as IID_IAsyncOperation_1_Windows_Devices_Spi_SpiDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::spi::SpiDevice> => [0xa88a28ba,0x6966,0x55e7,0x8c,0x81,0x7c,0x65,0xf7,0x4e,0x39,0xc0] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Spi_SpiDevice }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::spi::SpiController> => [0xb6b0df6f,0xc097,0x5844,0x93,0xbd,0x78,0x21,0x99,0x8f,0xdb,0x8e] as IID_IAsyncOperation_1_Windows_Devices_Spi_SpiController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::spi::SpiController> => [0x5e94d949,0xa844,0x5b25,0xa3,0xcc,0xaf,0xab,0xeb,0x18,0xc1,0xd2] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Spi_SpiController }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::spi::SpiController>> => [0x89624331,0xf802,0x56f7,0x9b,0x33,0x17,0xc6,0x16,0xec,0xbc,0xfa] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Spi_SpiController }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::spi::SpiController>> => [0xc8afc9cb,0x6807,0x57ec,0x84,0xc9,0x9f,0x3d,0xbc,0x00,0x34,0x50] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Spi_SpiController }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::gpio::GpioPin, &'a ::rt::gen::windows::devices::gpio::GpioPinValueChangedEventArgs> => [0x44ba689b,0x7d42,0x5374,0xad,0xd9,0xab,0x41,0xe8,0x77,0xa3,0x4b] as IID_TypedEventHandler_2_Windows_Devices_Gpio_GpioPin_Windows_Devices_Gpio_GpioPinValueChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::printers::Print3DDevice> => [0x7cfc4a8f,0x5eb7,0x5af7,0xbc,0x9f,0x78,0xa7,0xe4,0x07,0xcd,0x2e] as IID_IAsyncOperation_1_Windows_Devices_Printers_Print3DDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::printers::Print3DDevice> => [0x8d4b123f,0x4343,0x5195,0xbb,0xc9,0xb9,0x9e,0x95,0x6e,0x05,0x7f] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Printers_Print3DDevice }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType> => [0xc4a57c5e,0x32b0,0x55b3,0xad,0x13,0xce,0x1c,0x23,0x04,0x1e,0xd6] as IID_IAsyncOperation_1_Windows_Storage_Streams_IRandomAccessStreamWithContentType }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::streams::IRandomAccessStreamWithContentType> => [0x3dddecf4,0x1d39,0x58e8,0x83,0xb1,0xdb,0xed,0x54,0x1c,0x7f,0x35] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_Streams_IRandomAccessStreamWithContentType }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::power::Battery, &'a IInspectable> => [0x4d4aa646,0x767f,0x5645,0xaf,0x5c,0x54,0x64,0x64,0xd3,0xec,0x09] as IID_TypedEventHandler_2_Windows_Devices_Power_Battery_System_Object }
		RT_PINTERFACE!{ for IReference<i32> => [0x548cefbd,0xbc8a,0x5fa0,0x8d,0xf2,0x95,0x74,0x40,0xfc,0x8b,0xf4] as IID_IReference_1_System_Int32 }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::power::Battery> => [0xdaa3d556,0x1529,0x56d2,0xa5,0xf8,0xbf,0xb6,0xc2,0x2a,0x3d,0xfe] as IID_IAsyncOperation_1_Windows_Devices_Power_Battery }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::power::Battery> => [0x97f82115,0x3822,0x507b,0x82,0xe6,0x27,0x77,0xb3,0x36,0xe9,0x8e] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Power_Battery }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::sms::SmsSendMessageResult> => [0xfc0a0b0f,0x4dcc,0x5257,0xbc,0x61,0x34,0x35,0xe3,0x02,0xce,0x1f] as IID_IAsyncOperation_1_Windows_Devices_Sms_SmsSendMessageResult }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::sms::SmsSendMessageResult> => [0xc7d5c6fe,0x9206,0x5eb1,0xab,0xc1,0xc1,0xbc,0x21,0x80,0x4e,0xeb] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Sms_SmsSendMessageResult }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sms::SmsDevice2, &'a IInspectable> => [0x3f3808e6,0x3dee,0x57a6,0xa8,0x8d,0xba,0xcf,0xb0,0x66,0xc7,0xfb] as IID_TypedEventHandler_2_Windows_Devices_Sms_SmsDevice2_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sms::SmsMessageRegistration, &'a ::rt::gen::windows::devices::sms::SmsMessageReceivedTriggerDetails> => [0x33f985c7,0xdcfa,0x531f,0x9c,0xce,0xee,0x5e,0x6c,0x26,0xb1,0xe6] as IID_TypedEventHandler_2_Windows_Devices_Sms_SmsMessageRegistration_Windows_Devices_Sms_SmsMessageReceivedTriggerDetails }
		RT_PINTERFACE!{ for IAsyncOperation<i32> => [0x968b9665,0x06ed,0x5774,0x8f,0x53,0x8e,0xde,0xab,0xd5,0xf7,0xb5] as IID_IAsyncOperation_1_System_Int32 }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<i32> => [0xd60cae9d,0x88cb,0x59f1,0x85,0x76,0x3f,0xba,0x44,0x79,0x6b,0xe8] as IID_AsyncOperationCompletedHandler_1_System_Int32 }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, &'a ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachmentStateChangedEventArgs> => [0x82a8561d,0x3693,0x5c90,0x8c,0xf2,0x58,0xde,0x83,0xd8,0x02,0x43] as IID_TypedEventHandler_2_Windows_Devices_AllJoyn_AllJoynBusAttachment_Windows_Devices_AllJoyn_AllJoynBusAttachmentStateChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, &'a ::rt::gen::windows::devices::alljoyn::AllJoynCredentialsRequestedEventArgs> => [0xab2196e7,0x4b41,0x53bd,0x95,0xb1,0xb8,0x0c,0xad,0x82,0x47,0x95] as IID_TypedEventHandler_2_Windows_Devices_AllJoyn_AllJoynBusAttachment_Windows_Devices_AllJoyn_AllJoynCredentialsRequestedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, &'a ::rt::gen::windows::devices::alljoyn::AllJoynCredentialsVerificationRequestedEventArgs> => [0x7ef99c96,0x51b1,0x5670,0xa4,0x1f,0xae,0x40,0x4f,0x2f,0xf5,0x3f] as IID_TypedEventHandler_2_Windows_Devices_AllJoyn_AllJoynBusAttachment_Windows_Devices_AllJoyn_AllJoynCredentialsVerificationRequestedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::alljoyn::AllJoynBusAttachment, &'a ::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationCompleteEventArgs> => [0xe4fd18b4,0x4cde,0x508e,0x80,0x84,0x63,0xe7,0x28,0x32,0x62,0xc5] as IID_TypedEventHandler_2_Windows_Devices_AllJoyn_AllJoynBusAttachment_Windows_Devices_AllJoyn_AllJoynAuthenticationCompleteEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::security::cryptography::certificates::CertificateChain> => [0xf618c7d4,0xaee1,0x58ae,0xaf,0xe8,0xfc,0x33,0x6d,0xaf,0x03,0x95] as IID_IAsyncOperation_1_Windows_Security_Cryptography_Certificates_CertificateChain }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::security::cryptography::certificates::CertificateChain> => [0x4c3f50e9,0x90e3,0x5a30,0x90,0x15,0x4a,0xa0,0x37,0x69,0x04,0xf3] as IID_AsyncOperationCompletedHandler_1_Windows_Security_Cryptography_Certificates_CertificateChain }
		RT_PINTERFACE!{ for IReference<u8> => [0xe5198cc8,0x2873,0x55f5,0xb0,0xa1,0x84,0xff,0x9e,0x4a,0xad,0x62] as IID_IReference_1_System_Byte }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::networking::connectivity::ConnectionProfile> => [0x5bf519ca,0x8adb,0x5ab5,0xab,0xb8,0xff,0x1b,0xbe,0x5d,0x2d,0xe8] as IID_IAsyncOperation_1_Windows_Networking_Connectivity_ConnectionProfile }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::networking::connectivity::ConnectionProfile> => [0xe4f0c96a,0x0571,0x59f4,0xa9,0xa9,0xaf,0xac,0x3e,0x61,0xca,0xa0] as IID_AsyncOperationCompletedHandler_1_Windows_Networking_Connectivity_ConnectionProfile }
		RT_PINTERFACE!{ for IReference<u32> => [0x513ef3af,0xe784,0x5325,0xa9,0x1e,0x97,0xc2,0xb8,0x11,0x1c,0xf3] as IID_IReference_1_System_UInt32 }
		RT_PINTERFACE!{ for IReference<u64> => [0x6755e376,0x53bb,0x568b,0xa1,0x1d,0x17,0x23,0x98,0x68,0x30,0x9e] as IID_IReference_1_System_UInt64 }
		RT_PINTERFACE!{ for IReference<::rt::gen::windows::foundation::DateTime> => [0x5541d8a7,0x497c,0x5aa4,0x86,0xfc,0x77,0x13,0xad,0xbf,0x2a,0x2c] as IID_IReference_1_Windows_Foundation_DateTime }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::alljoyn::AllJoynAboutDataView> => [0x3757414e,0xf54b,0x51c4,0x8f,0x2f,0xe0,0x47,0x75,0x59,0xb2,0xad] as IID_IAsyncOperation_1_Windows_Devices_AllJoyn_AllJoynAboutDataView }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::alljoyn::AllJoynAboutDataView> => [0xba2da2f5,0xf9b0,0x5c66,0x8f,0xc9,0x7d,0x43,0x7a,0x67,0xf2,0x8a] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_AllJoyn_AllJoynAboutDataView }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::bluetooth::BluetoothDevice, &'a IInspectable> => [0xdb56ce1c,0x5e9f,0x5138,0x92,0x27,0xb1,0xa6,0x6d,0x60,0xbc,0x1b] as IID_TypedEventHandler_2_Windows_Devices_Bluetooth_BluetoothDevice_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::bluetooth::BluetoothDevice> => [0xb58d8d19,0x44bd,0x5ac0,0xa0,0xd6,0x1b,0x50,0x80,0x0f,0x31,0x81] as IID_IAsyncOperation_1_Windows_Devices_Bluetooth_BluetoothDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::bluetooth::BluetoothDevice> => [0xb2e8cdd1,0x66aa,0x5892,0x85,0xa3,0x8f,0x0b,0x16,0x5e,0x43,0xfc] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Bluetooth_BluetoothDevice }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::bluetooth::BluetoothLEDevice, &'a IInspectable> => [0xa90661e2,0x372e,0x5d1e,0xbb,0xbb,0xb8,0xa2,0xce,0x0e,0x7c,0x4d] as IID_TypedEventHandler_2_Windows_Devices_Bluetooth_BluetoothLEDevice_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::bluetooth::BluetoothLEDevice> => [0x375f9d67,0x74a2,0x5f91,0xa1,0x1d,0x16,0x90,0x93,0x71,0x8d,0x41] as IID_IAsyncOperation_1_Windows_Devices_Bluetooth_BluetoothLEDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::bluetooth::BluetoothLEDevice> => [0x9156b79f,0xc54a,0x5277,0x8f,0x8b,0xd2,0xcc,0x43,0xc7,0xe0,0x04] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Bluetooth_BluetoothLEDevice }
		RT_PINTERFACE!{ for IReference<i16> => [0x6ec9e41b,0x6709,0x5647,0x99,0x18,0xa1,0x27,0x01,0x10,0xfc,0x4e] as IID_IReference_1_System_Int16 }
		RT_PINTERFACE!{ for IReference<::rt::gen::windows::foundation::TimeSpan> => [0x604d0c4c,0x91de,0x5c2a,0x93,0x5f,0x36,0x2f,0x13,0xea,0xf8,0x00] as IID_IReference_1_Windows_Foundation_TimeSpan }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService> => [0xe5e90272,0x408f,0x5055,0x9b,0xd3,0x88,0x40,0x89,0x82,0xd3,0x01] as IID_IAsyncOperation_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattDeviceService }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService> => [0x2dbcf64a,0x262b,0x5708,0xad,0xb1,0xc3,0xb8,0x75,0x0b,0xd6,0x80] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattDeviceService }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadResult> => [0xd40432a8,0x1e14,0x51d0,0xb4,0x9b,0xae,0x2c,0xe1,0xaa,0x05,0xe5] as IID_IAsyncOperation_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattReadResult }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadResult> => [0xd8992aa0,0xeac2,0x55b7,0x92,0xc5,0x89,0x48,0x86,0xbe,0xb0,0xca] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattReadResult }
		RT_PINTERFACE!{ for IAsyncOperation<::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus> => [0x3ff69516,0x1bfb,0x52e9,0x9e,0xe6,0xe5,0xcd,0xb7,0x8e,0x16,0x83] as IID_IAsyncOperation_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattCommunicationStatus }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCommunicationStatus> => [0x2154117a,0x978d,0x59db,0x99,0xcf,0x6b,0x69,0x0c,0xb3,0x38,0x9b] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattCommunicationStatus }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadClientCharacteristicConfigurationDescriptorResult> => [0xcf4444cc,0x4077,0x5719,0x83,0x66,0x46,0xe8,0x6b,0x98,0x36,0x85] as IID_IAsyncOperation_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattReadClientCharacteristicConfigurationDescriptorResult }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattReadClientCharacteristicConfigurationDescriptorResult> => [0x98f9a6f3,0x4d29,0x5351,0x8b,0x12,0x75,0x1d,0xc9,0x77,0xa3,0x31] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattReadClientCharacteristicConfigurationDescriptorResult }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic, &'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattValueChangedEventArgs> => [0xc1f420f6,0x6292,0x5760,0xa2,0xc9,0x9d,0xdf,0x98,0x68,0x3c,0xfc] as IID_TypedEventHandler_2_Windows_Devices_Bluetooth_GenericAttributeProfile_GattCharacteristic_Windows_Devices_Bluetooth_GenericAttributeProfile_GattValueChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommDeviceService> => [0x0df56bd7,0xc8f6,0x5c32,0x96,0x44,0xaa,0x0b,0xcf,0x28,0xd7,0x8c] as IID_IAsyncOperation_1_Windows_Devices_Bluetooth_Rfcomm_RfcommDeviceService }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommDeviceService> => [0x5c772518,0x442f,0x58ed,0x80,0xcb,0x53,0x8d,0x34,0xb8,0x82,0x95] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Bluetooth_Rfcomm_RfcommDeviceService }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IMapView<u32, &'a ::rt::gen::windows::storage::streams::IBuffer>> => [0xd4904ded,0xbc1d,0x5933,0xae,0xcf,0xe4,0x2c,0x5d,0x46,0x5b,0xff] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IMapView_2_System_UInt32_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IMapView<u32, &'a ::rt::gen::windows::storage::streams::IBuffer>> => [0x92c2e4d0,0x7c25,0x596b,0x91,0x35,0x10,0xd1,0x47,0x2e,0x69,0x68] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IMapView_2_System_UInt32_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceProvider> => [0xfed44828,0xe232,0x554d,0x85,0xd1,0x2f,0x04,0xd1,0x32,0x2e,0x69] as IID_IAsyncOperation_1_Windows_Devices_Bluetooth_Rfcomm_RfcommServiceProvider }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommServiceProvider> => [0x446a7f50,0x8f2e,0x51f0,0xae,0xbb,0x1b,0xc3,0xd1,0x92,0x90,0x5f] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Bluetooth_Rfcomm_RfcommServiceProvider }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::networking::sockets::StreamSocketListener, &'a ::rt::gen::windows::networking::sockets::StreamSocketListenerConnectionReceivedEventArgs> => [0x33d00d41,0xc94f,0x5a61,0x9a,0xb7,0x28,0x0d,0xce,0xfa,0x0b,0x08] as IID_TypedEventHandler_2_Windows_Networking_Sockets_StreamSocketListener_Windows_Networking_Sockets_StreamSocketListenerConnectionReceivedEventArgs }
		RT_PINTERFACE!{ for IAsyncOperationWithProgress<u32, u32> => [0xeccb574a,0xc684,0x5572,0xa6,0x79,0x6b,0x08,0x42,0xcf,0xb5,0x7f] as IID_IAsyncOperationWithProgress_2_System_UInt32_System_UInt32 }
		RT_PINTERFACE!{ for AsyncOperationProgressHandler<u32, u32> => [0xea0fe405,0xd432,0x5ac7,0x9e,0xf8,0x5a,0x65,0xe1,0xf9,0x7d,0x7e] as IID_AsyncOperationProgressHandler_2_System_UInt32_System_UInt32 }
		RT_PINTERFACE!{ for AsyncOperationWithProgressCompletedHandler<u32, u32> => [0x1e466dc5,0x840f,0x54f9,0xb8,0x77,0x5e,0x3a,0x9f,0x4b,0x6c,0x74] as IID_AsyncOperationWithProgressCompletedHandler_2_System_UInt32_System_UInt32 }
		RT_PINTERFACE!{ for IAsyncOperation<bool> => [0xcdb5efb3,0x5788,0x509d,0x9b,0xe1,0x71,0xcc,0xb8,0xa3,0x36,0x2a] as IID_IAsyncOperation_1_System_Boolean }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<bool> => [0xc1d3d1a2,0xae17,0x5a5f,0xb5,0xa2,0xbd,0xcc,0x88,0x44,0x88,0x9a] as IID_AsyncOperationCompletedHandler_1_System_Boolean }
		RT_PINTERFACE!{ for<'a> IAsyncOperationWithProgress<&'a ::rt::gen::windows::storage::streams::IBuffer, u32> => [0xd26b2819,0x897f,0x5c7d,0x84,0xd6,0x56,0xd7,0x96,0x56,0x14,0x31] as IID_IAsyncOperationWithProgress_2_Windows_Storage_Streams_IBuffer_System_UInt32 }
		RT_PINTERFACE!{ for<'a> AsyncOperationProgressHandler<&'a ::rt::gen::windows::storage::streams::IBuffer, u32> => [0xbf666554,0x7605,0x5d9a,0xb1,0x4e,0x18,0xd8,0xc8,0x47,0x2a,0xfe] as IID_AsyncOperationProgressHandler_2_Windows_Storage_Streams_IBuffer_System_UInt32 }
		RT_PINTERFACE!{ for<'a> AsyncOperationWithProgressCompletedHandler<&'a ::rt::gen::windows::storage::streams::IBuffer, u32> => [0x06386a7a,0xe009,0x5b0b,0xab,0x68,0xa8,0xe4,0x8b,0x51,0x66,0x47] as IID_AsyncOperationWithProgressCompletedHandler_2_Windows_Storage_Streams_IBuffer_System_UInt32 }
		RT_PINTERFACE!{ for IReference<::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementFlags> => [0x91c0ba96,0x9e69,0x5b82,0xbf,0x1d,0x83,0xab,0x2a,0x50,0x9c,0x53] as IID_IReference_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementFlags }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementWatcher, &'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementReceivedEventArgs> => [0x90eb4eca,0xd465,0x5ea0,0xa6,0x1c,0x03,0x3c,0x8c,0x5e,0xce,0xf2] as IID_TypedEventHandler_2_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementWatcher_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementWatcher, &'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementWatcherStoppedEventArgs> => [0x9936a4db,0xdc99,0x55c3,0x9e,0x9b,0xbf,0x48,0x54,0xbd,0x9e,0xab] as IID_TypedEventHandler_2_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementWatcher_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementWatcherStoppedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementPublisher, &'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementPublisherStatusChangedEventArgs> => [0xc2ffa4f1,0x5893,0x54a8,0xbd,0x94,0xaa,0x11,0x98,0xb0,0x5d,0x07] as IID_TypedEventHandler_2_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementPublisher_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementPublisherStatusChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::DevicePicker, &'a ::rt::gen::windows::devices::enumeration::DeviceSelectedEventArgs> => [0x47e48c88,0x1c56,0x5b58,0x96,0xa2,0x8e,0x81,0x3d,0x25,0x07,0x7a] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_DevicePicker_Windows_Devices_Enumeration_DeviceSelectedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::DevicePicker, &'a ::rt::gen::windows::devices::enumeration::DeviceDisconnectButtonClickedEventArgs> => [0x35dd0319,0x5723,0x506c,0x88,0x96,0x1a,0x28,0xb8,0x2b,0xe7,0x98] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_DevicePicker_Windows_Devices_Enumeration_DeviceDisconnectButtonClickedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::DevicePicker, &'a IInspectable> => [0x62c6d98c,0x57ee,0x5bb8,0xa4,0x1c,0x95,0x8d,0x20,0xc3,0xf3,0xe8] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_DevicePicker_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::enumeration::DeviceInformation> => [0x07faa053,0xeb2f,0x5cba,0xb2,0x5b,0xd9,0xd5,0x7b,0xe6,0x71,0x5f] as IID_IAsyncOperation_1_Windows_Devices_Enumeration_DeviceInformation }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceInformation> => [0xbb483df2,0x7bb6,0x5923,0xa2,0x8d,0x83,0x42,0xec,0x30,0x04,0x6b] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Enumeration_DeviceInformation }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceWatcher, &'a ::rt::gen::windows::devices::enumeration::DeviceInformation> => [0x03c5a07b,0x990c,0x5d09,0xb0,0xb8,0x57,0x34,0xea,0xa3,0x82,0x22] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_DeviceWatcher_Windows_Devices_Enumeration_DeviceInformation }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceWatcher, &'a ::rt::gen::windows::devices::enumeration::DeviceInformationUpdate> => [0x906f1254,0x79ad,0x54fc,0x93,0xc4,0xcd,0xb9,0x9b,0x43,0x78,0x99] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_DeviceWatcher_Windows_Devices_Enumeration_DeviceInformationUpdate }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceWatcher, &'a IInspectable> => [0x9234630f,0x1ff4,0x54f6,0x9e,0x3f,0xac,0x20,0x36,0x9b,0x77,0x25] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_DeviceWatcher_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::enumeration::DeviceInformationCollection> => [0x45180254,0x082e,0x5274,0xb2,0xe7,0xac,0x05,0x17,0xf4,0x4d,0x07] as IID_IAsyncOperation_1_Windows_Devices_Enumeration_DeviceInformationCollection }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceInformationCollection> => [0x4a458732,0x527e,0x5c73,0x9a,0x68,0xa7,0x3d,0xa3,0x70,0xf7,0x82] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Enumeration_DeviceInformationCollection }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::enumeration::DeviceThumbnail> => [0xbac083a3,0x3a19,0x5072,0x9d,0x90,0x13,0x33,0x23,0xa0,0x49,0xba] as IID_IAsyncOperation_1_Windows_Devices_Enumeration_DeviceThumbnail }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceThumbnail> => [0x86d455b2,0xd795,0x554c,0x9c,0x31,0xbf,0x65,0x39,0x34,0x9c,0x19] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Enumeration_DeviceThumbnail }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::enumeration::DevicePairingResult> => [0x1002db74,0x8948,0x591e,0x81,0x5d,0xe4,0x0b,0x66,0x75,0x99,0xa3] as IID_IAsyncOperation_1_Windows_Devices_Enumeration_DevicePairingResult }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::enumeration::DevicePairingResult> => [0x7ee0247f,0x5f57,0x5cb2,0xb4,0x0e,0x18,0xb5,0xa2,0x11,0xd6,0xc3] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Enumeration_DevicePairingResult }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceInformationCustomPairing, &'a ::rt::gen::windows::devices::enumeration::DevicePairingRequestedEventArgs> => [0xfa65231f,0x4178,0x5de1,0xb2,0xcc,0x03,0xe2,0x2d,0x77,0x02,0xb4] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_DeviceInformationCustomPairing_Windows_Devices_Enumeration_DevicePairingRequestedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::enumeration::DeviceUnpairingResult> => [0x2bb4df3d,0xbd7e,0x5fe0,0x90,0x20,0x56,0xdc,0x0d,0x30,0xb9,0x35] as IID_IAsyncOperation_1_Windows_Devices_Enumeration_DeviceUnpairingResult }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceUnpairingResult> => [0x9bbe6eb9,0xdb2d,0x5160,0xa2,0x0c,0xf0,0xc2,0x65,0xf2,0x0d,0x8e] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Enumeration_DeviceUnpairingResult }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::DeviceAccessInformation, &'a ::rt::gen::windows::devices::enumeration::DeviceAccessChangedEventArgs> => [0x4c71d028,0xb793,0x5bce,0xae,0x59,0xfa,0x77,0xf4,0x5a,0x40,0xd8] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_DeviceAccessInformation_Windows_Devices_Enumeration_DeviceAccessChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher, &'a ::rt::gen::windows::devices::enumeration::pnp::PnpObject> => [0xd578eed2,0x58e5,0x5825,0x8a,0xf2,0x12,0xf8,0x93,0x87,0xb6,0x56] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_Pnp_PnpObjectWatcher_Windows_Devices_Enumeration_Pnp_PnpObject }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher, &'a ::rt::gen::windows::devices::enumeration::pnp::PnpObjectUpdate> => [0xaf8f929d,0x8058,0x5c38,0xa3,0xd8,0x30,0xaa,0x7a,0x08,0xb5,0x88] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_Pnp_PnpObjectWatcher_Windows_Devices_Enumeration_Pnp_PnpObjectUpdate }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObjectWatcher, &'a IInspectable> => [0x2ee2b4c9,0xb696,0x5ecc,0xb2,0x9b,0xf1,0xe0,0xef,0x5f,0xe1,0xf7] as IID_TypedEventHandler_2_Windows_Devices_Enumeration_Pnp_PnpObjectWatcher_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObject> => [0x22b0fb93,0x30e6,0x501a,0xbd,0x3b,0x9f,0xa3,0x06,0x3e,0x9c,0x16] as IID_IAsyncOperation_1_Windows_Devices_Enumeration_Pnp_PnpObject }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObject> => [0x9d615463,0x6879,0x521f,0x8e,0x97,0xe6,0x6d,0x3d,0xdb,0xc9,0x5e] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Enumeration_Pnp_PnpObject }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObjectCollection> => [0xf383c2cc,0xf326,0x5bbe,0x95,0xd1,0xcb,0xc2,0x47,0x14,0xef,0x86] as IID_IAsyncOperation_1_Windows_Devices_Enumeration_Pnp_PnpObjectCollection }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObjectCollection> => [0x811d834c,0xa15e,0x5522,0xb7,0xf4,0xe5,0x30,0x04,0xfc,0x58,0xff] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Enumeration_Pnp_PnpObjectCollection }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::geolocation::geofencing::GeofenceMonitor, &'a IInspectable> => [0xecc5af2c,0xe47a,0x59ce,0x86,0xbe,0x9c,0x30,0x66,0xfe,0x26,0xf7] as IID_TypedEventHandler_2_Windows_Devices_Geolocation_Geofencing_GeofenceMonitor_System_Object }
		RT_PINTERFACE!{ for IReference<f64> => [0x2f2d6c29,0x5473,0x5f3e,0x92,0xe7,0x96,0x57,0x2b,0xb9,0x90,0xe2] as IID_IReference_1_System_Double }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::geolocation::Geoposition> => [0xee73ecf0,0x099d,0x57e5,0x84,0x07,0x5b,0x32,0xe5,0xaf,0x1c,0xc4] as IID_IAsyncOperation_1_Windows_Devices_Geolocation_Geoposition }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::geolocation::Geoposition> => [0x7668a704,0x244e,0x5e12,0x8d,0xcb,0x92,0xa3,0x29,0x9e,0xba,0x26] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Geolocation_Geoposition }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::geolocation::Geolocator, &'a ::rt::gen::windows::devices::geolocation::PositionChangedEventArgs> => [0xdf3c6164,0x4e7b,0x5e8e,0x9a,0x7e,0x13,0xda,0x05,0x9d,0xec,0x1e] as IID_TypedEventHandler_2_Windows_Devices_Geolocation_Geolocator_Windows_Devices_Geolocation_PositionChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::geolocation::Geolocator, &'a ::rt::gen::windows::devices::geolocation::StatusChangedEventArgs> => [0x97fcf582,0xde6b,0x5cd3,0x96,0x90,0xe2,0xec,0xbb,0x66,0xda,0x4d] as IID_TypedEventHandler_2_Windows_Devices_Geolocation_Geolocator_Windows_Devices_Geolocation_StatusChangedEventArgs }
		RT_PINTERFACE!{ for IAsyncOperation<::rt::gen::windows::devices::geolocation::GeolocationAccessStatus> => [0xde2b24d0,0xb726,0x57b1,0xa7,0xc5,0xe5,0xa1,0x39,0x32,0xb7,0xde] as IID_IAsyncOperation_1_Windows_Devices_Geolocation_GeolocationAccessStatus }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<::rt::gen::windows::devices::geolocation::GeolocationAccessStatus> => [0xf3524c93,0xe5c7,0x5b88,0xbe,0xdb,0xd3,0xe6,0x37,0xcf,0xf2,0x71] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Geolocation_GeolocationAccessStatus }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::geolocation::Geoposition>> => [0x9454b533,0xefb4,0x5385,0x8d,0x3a,0x43,0x7f,0xab,0xc3,0x2d,0x91] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Geolocation_Geoposition }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::geolocation::Geoposition>> => [0x6c67a1d1,0x9441,0x5aee,0xb6,0x25,0xe3,0xc1,0xb5,0x67,0x6a,0x6d] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Geolocation_Geoposition }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidDevice> => [0xa76a4fbf,0x5177,0x5256,0x84,0xa8,0xb3,0x1a,0x8d,0xcf,0x10,0x48] as IID_IAsyncOperation_1_Windows_Devices_HumanInterfaceDevice_HidDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidDevice> => [0xb0e8e149,0x0cb6,0x55a7,0xbc,0xc1,0xd9,0x96,0x32,0x4d,0x65,0xc4] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_HumanInterfaceDevice_HidDevice }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::input::MouseDevice, &'a ::rt::gen::windows::devices::input::MouseEventArgs> => [0x5d72e594,0x28e4,0x5895,0xa3,0x4b,0xea,0x91,0x0f,0x70,0xfd,0xbb] as IID_TypedEventHandler_2_Windows_Devices_Input_MouseDevice_Windows_Devices_Input_MouseEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::lights::Lamp> => [0x52a69dfd,0xf0d6,0x5931,0xb8,0xe1,0xf3,0x80,0x66,0xd7,0x1b,0xf2] as IID_IAsyncOperation_1_Windows_Devices_Lights_Lamp }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::lights::Lamp> => [0x191a8c6e,0x60dd,0x5a21,0xa5,0x3c,0xbf,0x3f,0x94,0x0a,0x1d,0xde] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Lights_Lamp }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::midi::MidiInPort> => [0xcc664f0e,0xedb1,0x55c8,0x9e,0xf7,0xec,0x90,0x07,0xe4,0x56,0x1c] as IID_IAsyncOperation_1_Windows_Devices_Midi_MidiInPort }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::midi::MidiInPort> => [0x6c090fb2,0x8099,0x558f,0x8a,0x92,0x9a,0x8e,0xa8,0x06,0xe6,0xfb] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Midi_MidiInPort }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::midi::IMidiOutPort> => [0x32699a4d,0x1cc0,0x5a1c,0x9d,0xa6,0x87,0x51,0x97,0x87,0x50,0x86] as IID_IAsyncOperation_1_Windows_Devices_Midi_IMidiOutPort }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::midi::IMidiOutPort> => [0xeed37805,0x2a49,0x59b4,0xb4,0xd4,0x11,0x88,0xc6,0x81,0x91,0x22] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Midi_IMidiOutPort }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::midi::MidiSynthesizer> => [0x9388b978,0x13f1,0x5e37,0x81,0x33,0x94,0x43,0x0d,0x90,0xdd,0x50] as IID_IAsyncOperation_1_Windows_Devices_Midi_MidiSynthesizer }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::midi::MidiSynthesizer> => [0x5d716335,0xd087,0x516f,0xad,0x0a,0x63,0xf6,0x1c,0xbc,0xf3,0x42] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Midi_MidiSynthesizer }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSourceWatcher, &'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSourceAddedEventArgs> => [0x7202e817,0x22b8,0x5e7b,0x86,0xb5,0xc4,0xa9,0x0c,0xcc,0x23,0xaa] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionColorFrameSourceWatcher_Windows_Devices_Perception_PerceptionColorFrameSourceAddedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSourceWatcher, &'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSourceRemovedEventArgs> => [0xc7131ddd,0xff22,0x5fb7,0xad,0xa0,0x96,0x1e,0x3b,0x28,0x91,0x7b] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionColorFrameSourceWatcher_Windows_Devices_Perception_PerceptionColorFrameSourceRemovedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSourceWatcher, &'a IInspectable> => [0xb3f5bf64,0x7fc0,0x5d8c,0x97,0x8c,0xa3,0x8d,0x5b,0x18,0xa5,0x1d] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionColorFrameSourceWatcher_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceWatcher, &'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceAddedEventArgs> => [0xa3c1e25f,0x3574,0x5a27,0xa7,0x91,0x16,0xe1,0xbc,0xc4,0x24,0xf4] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionDepthFrameSourceWatcher_Windows_Devices_Perception_PerceptionDepthFrameSourceAddedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceWatcher, &'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceRemovedEventArgs> => [0x5cf5faf7,0x0785,0x5589,0xa6,0x21,0x08,0x9d,0x90,0x04,0x12,0xc8] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionDepthFrameSourceWatcher_Windows_Devices_Perception_PerceptionDepthFrameSourceRemovedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSourceWatcher, &'a IInspectable> => [0xc5efa976,0xd948,0x50c7,0x85,0x55,0x66,0x41,0x90,0xf9,0xa9,0x68] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionDepthFrameSourceWatcher_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceWatcher, &'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceAddedEventArgs> => [0xa8180cd1,0xc25b,0x5c7f,0x94,0xdd,0x19,0x84,0x23,0xbb,0x56,0xd5] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionInfraredFrameSourceWatcher_Windows_Devices_Perception_PerceptionInfraredFrameSourceAddedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceWatcher, &'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceRemovedEventArgs> => [0x2ca3a9b7,0x3348,0x5953,0x8d,0x0d,0xef,0x8d,0x78,0x64,0x0b,0x23] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionInfraredFrameSourceWatcher_Windows_Devices_Perception_PerceptionInfraredFrameSourceRemovedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSourceWatcher, &'a IInspectable> => [0x1555a628,0x3dff,0x5fd0,0xb1,0x0a,0xca,0x6a,0xdb,0x24,0x40,0xc6] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionInfraredFrameSourceWatcher_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource>> => [0xb11eecaa,0x6f8c,0x5040,0x8d,0x46,0xc3,0x20,0x4c,0x56,0x25,0x82] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Perception_PerceptionInfraredFrameSource }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource>> => [0x3aac58a8,0x4454,0x57e5,0xa9,0x0b,0x24,0x49,0xc5,0xb7,0xdf,0xe8] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Perception_PerceptionInfraredFrameSource }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource> => [0x55122e42,0xcc65,0x5ccd,0x8f,0x6c,0x84,0xce,0xd0,0x9d,0xb2,0x4e] as IID_IAsyncOperation_1_Windows_Devices_Perception_PerceptionInfraredFrameSource }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource> => [0xa8d4cd8e,0xb210,0x54f7,0xae,0x2b,0x77,0x70,0xe1,0x9b,0x3e,0x36] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Perception_PerceptionInfraredFrameSource }
		RT_PINTERFACE!{ for IAsyncOperation<::rt::gen::windows::devices::perception::PerceptionFrameSourceAccessStatus> => [0x2c2f22a8,0xf383,0x5802,0xba,0x2c,0x0c,0xbb,0xcd,0x98,0x9c,0x9a] as IID_IAsyncOperation_1_Windows_Devices_Perception_PerceptionFrameSourceAccessStatus }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<::rt::gen::windows::devices::perception::PerceptionFrameSourceAccessStatus> => [0x62744ea4,0x3447,0x5722,0xab,0x5e,0x02,0x56,0x7b,0x4f,0xce,0xeb] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Perception_PerceptionFrameSourceAccessStatus }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource>> => [0xe20a534d,0xd406,0x5964,0x84,0x65,0xe6,0xdc,0x75,0xc5,0x82,0x1d] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Perception_PerceptionDepthFrameSource }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource>> => [0xc06e62a4,0x965b,0x5a29,0x97,0x32,0x8a,0xc8,0x66,0x9b,0x58,0x5e] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Perception_PerceptionDepthFrameSource }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource> => [0x4382b038,0xd4b1,0x540b,0x85,0x9a,0x70,0x16,0x62,0x6b,0xb9,0x9d] as IID_IAsyncOperation_1_Windows_Devices_Perception_PerceptionDepthFrameSource }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource> => [0xb48cb886,0x3476,0x58d9,0xb7,0x6d,0xfd,0xa6,0xb3,0xe8,0x1f,0x54] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Perception_PerceptionDepthFrameSource }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource>> => [0x33845b5f,0xd59e,0x5271,0xbb,0x68,0xf7,0x4e,0x9d,0x6a,0x53,0x8d] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Perception_PerceptionColorFrameSource }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource>> => [0x0a36a7af,0xda9e,0x553f,0x8d,0xc5,0xe8,0x9d,0x70,0x5b,0xb4,0x0b] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Perception_PerceptionColorFrameSource }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource> => [0x9647fec8,0x2c56,0x5348,0x86,0xc8,0xa9,0xc3,0xa9,0x7f,0xb9,0x44] as IID_IAsyncOperation_1_Windows_Devices_Perception_PerceptionColorFrameSource }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource> => [0x3b56acc2,0xe275,0x54fb,0xbe,0x08,0x9f,0xdc,0x8f,0x1a,0x1e,0x10] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Perception_PerceptionColorFrameSource }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource, &'a IInspectable> => [0x023444d9,0x7b47,0x5497,0x95,0x69,0x43,0x99,0xfa,0xf9,0x67,0x17] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionColorFrameSource_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource, &'a ::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertiesChangedEventArgs> => [0xb2c3a488,0x7ed9,0x5110,0x80,0x4a,0x97,0x92,0xef,0x7f,0x26,0xbe] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionColorFrameSource_Windows_Devices_Perception_PerceptionFrameSourcePropertiesChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCameraIntrinsics> => [0x702b0f49,0xa742,0x5c3a,0xab,0xd6,0x77,0xf9,0x99,0x9b,0x8a,0x09] as IID_IAsyncOperation_1_Windows_Devices_Perception_PerceptionDepthCorrelatedCameraIntrinsics }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCameraIntrinsics> => [0xf396602a,0x3d8d,0x5fd5,0x99,0xe3,0x1d,0x36,0x30,0xbe,0x59,0x38] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Perception_PerceptionDepthCorrelatedCameraIntrinsics }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCoordinateMapper> => [0xf04b9d99,0xc0d5,0x5b48,0x9a,0xe5,0x98,0x02,0x09,0x3c,0xb4,0x5e] as IID_IAsyncOperation_1_Windows_Devices_Perception_PerceptionDepthCorrelatedCoordinateMapper }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthCorrelatedCoordinateMapper> => [0x48deeda0,0x684d,0x51e6,0xb0,0x7c,0xd2,0x34,0xd1,0x00,0x6b,0xfc] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Perception_PerceptionDepthCorrelatedCoordinateMapper }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeResult> => [0x4a7bcb69,0x2b09,0x55d1,0xaf,0x68,0xb5,0xaa,0x5c,0x2c,0x64,0x71] as IID_IAsyncOperation_1_Windows_Devices_Perception_PerceptionFrameSourcePropertyChangeResult }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertyChangeResult> => [0x3a06099c,0xdba6,0x58a5,0x84,0x64,0xe2,0x32,0x68,0x96,0x84,0x1a] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Perception_PerceptionFrameSourcePropertyChangeResult }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, &'a IInspectable> => [0x135ba76a,0xab44,0x5f69,0xb2,0x08,0xe7,0x32,0xcc,0xe9,0x40,0x3b] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionDepthFrameSource_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource, &'a ::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertiesChangedEventArgs> => [0x188fdef2,0xd829,0x548b,0xa8,0x9d,0x38,0xa3,0x4c,0x5c,0xb6,0x41] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionDepthFrameSource_Windows_Devices_Perception_PerceptionFrameSourcePropertiesChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource, &'a IInspectable> => [0x31edabbd,0xd123,0x5e88,0x89,0xd8,0xc8,0x0e,0xe8,0xf0,0xf2,0xca] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionInfraredFrameSource_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource, &'a ::rt::gen::windows::devices::perception::PerceptionFrameSourcePropertiesChangedEventArgs> => [0xb6c1b828,0xa157,0x54ed,0x9c,0x18,0x69,0x19,0xb9,0xf9,0x1b,0xe9] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionInfraredFrameSource_Windows_Devices_Perception_PerceptionFrameSourcePropertiesChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pointofservice::BarcodeScanner> => [0x616494dd,0x30a2,0x523f,0xb1,0xa2,0x9a,0x11,0x74,0xde,0x3b,0x17] as IID_IAsyncOperation_1_Windows_Devices_PointOfService_BarcodeScanner }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pointofservice::BarcodeScanner> => [0x8d8abf39,0x99dd,0x50a4,0xaa,0x7c,0x2f,0x73,0x01,0xb5,0xca,0x9c] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_PointOfService_BarcodeScanner }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner> => [0xc9704809,0xa0e2,0x5e1d,0x87,0xcf,0x75,0xa1,0xd4,0x9d,0x7b,0xdb] as IID_IAsyncOperation_1_Windows_Devices_PointOfService_ClaimedBarcodeScanner }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner> => [0xff72ba2d,0xf3c4,0x5abe,0xbb,0xce,0x53,0x15,0x04,0x49,0xb6,0x37] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_PointOfService_ClaimedBarcodeScanner }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a str> => [0x3e1fe603,0xf897,0x5263,0xb3,0x28,0x08,0x06,0x42,0x6b,0x8a,0x79] as IID_IAsyncOperation_1_System_String }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a str> => [0xb79a741f,0x7fb5,0x50ae,0x9e,0x99,0x91,0x12,0x01,0xec,0x3d,0x41] as IID_AsyncOperationCompletedHandler_1_System_String }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<u32>> => [0x52c56f3c,0x713a,0x5162,0x9e,0x62,0x36,0x2c,0xe7,0xed,0x53,0xbe] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_System_UInt32 }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<u32>> => [0x55772f29,0xda64,0x5c87,0x87,0x1c,0x07,0x43,0x37,0xa8,0x45,0x73] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_System_UInt32 }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::streams::IBuffer> => [0x3bee8834,0xb9a7,0x5a80,0xa7,0x46,0x5e,0xf0,0x97,0x22,0x78,0x78] as IID_IAsyncOperation_1_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::streams::IBuffer> => [0x51c3d2fd,0xb8a1,0x5620,0xb7,0x46,0x7e,0xe6,0xd5,0x33,0xac,0xa3] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::BarcodeScanner, &'a ::rt::gen::windows::devices::pointofservice::BarcodeScannerStatusUpdatedEventArgs> => [0xcfc8a053,0xf611,0x521a,0x8a,0xb0,0xa4,0x60,0xe1,0xf7,0x16,0x4d] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_BarcodeScanner_Windows_Devices_PointOfService_BarcodeScannerStatusUpdatedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pointofservice::MagneticStripeReader> => [0x93726e09,0x817c,0x5f33,0xbe,0xe4,0x09,0x0d,0xe7,0x07,0x4f,0x19] as IID_IAsyncOperation_1_Windows_Devices_PointOfService_MagneticStripeReader }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pointofservice::MagneticStripeReader> => [0x32c55f7b,0x8ee3,0x555d,0x99,0x8b,0x78,0xc9,0x8a,0xa9,0x62,0x7b] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_PointOfService_MagneticStripeReader }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader> => [0x41630bd4,0xf45a,0x590d,0x8a,0x4e,0xf7,0x0c,0x9e,0x49,0xad,0x01] as IID_IAsyncOperation_1_Windows_Devices_PointOfService_ClaimedMagneticStripeReader }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader> => [0x946c2d64,0x22d4,0x552d,0xab,0xfb,0x9e,0xb3,0x41,0xbd,0x67,0xf3] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_PointOfService_ClaimedMagneticStripeReader }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::MagneticStripeReader, &'a ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderStatusUpdatedEventArgs> => [0x722d8bfa,0xe10e,0x548d,0xaf,0x29,0x28,0xdd,0x90,0x6f,0x6f,0xc9] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_MagneticStripeReader_Windows_Devices_PointOfService_MagneticStripeReaderStatusUpdatedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pointofservice::PosPrinter> => [0x024f77ce,0x51c3,0x5afc,0x9f,0x30,0x24,0xb3,0xc0,0xf3,0xb2,0x5a] as IID_IAsyncOperation_1_Windows_Devices_PointOfService_PosPrinter }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pointofservice::PosPrinter> => [0x5e8dbbc8,0x8b60,0x5881,0x8b,0x6e,0xf6,0x99,0xb4,0x94,0x9d,0xba] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_PointOfService_PosPrinter }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pointofservice::ClaimedPosPrinter> => [0xb4476f95,0x355a,0x503d,0xb8,0x44,0x17,0x56,0xc8,0xcf,0xda,0x98] as IID_IAsyncOperation_1_Windows_Devices_PointOfService_ClaimedPosPrinter }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedPosPrinter> => [0x01eb0dc3,0x3c30,0x5eea,0x9f,0xce,0xef,0xb3,0x98,0xe0,0xbe,0x34] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_PointOfService_ClaimedPosPrinter }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::PosPrinter, &'a ::rt::gen::windows::devices::pointofservice::PosPrinterStatusUpdatedEventArgs> => [0x20b0c66a,0x5f41,0x5a32,0xb4,0x5a,0x34,0x4f,0x12,0xe7,0x0a,0x34] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_PosPrinter_Windows_Devices_PointOfService_PosPrinterStatusUpdatedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::graphics::imaging::ImageStream> => [0x684165be,0x0011,0x56d6,0xbe,0xbf,0x43,0x00,0x16,0xd5,0x1b,0x7a] as IID_IAsyncOperation_1_Windows_Graphics_Imaging_ImageStream }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::graphics::imaging::ImageStream> => [0x29bb8288,0x4462,0x516e,0xa6,0x75,0x8c,0x92,0x35,0xc4,0x29,0x94] as IID_AsyncOperationCompletedHandler_1_Windows_Graphics_Imaging_ImageStream }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::graphics::imaging::PixelDataProvider> => [0x8c2dfeb0,0x6c22,0x5863,0x88,0xd8,0x85,0xc1,0xfb,0xc7,0x56,0x97] as IID_IAsyncOperation_1_Windows_Graphics_Imaging_PixelDataProvider }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::graphics::imaging::PixelDataProvider> => [0x37bdf4be,0x2f39,0x592c,0xa4,0xf7,0xd1,0x6a,0x09,0xd2,0xb2,0xdb] as IID_AsyncOperationCompletedHandler_1_Windows_Graphics_Imaging_PixelDataProvider }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::graphics::imaging::BitmapPropertySet> => [0x464ac000,0xb1f1,0x5246,0x82,0x68,0x91,0x2a,0x25,0x93,0xd8,0x89] as IID_IAsyncOperation_1_Windows_Graphics_Imaging_BitmapPropertySet }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::graphics::imaging::BitmapPropertySet> => [0xa8325bd7,0xa3be,0x5881,0x9f,0xa7,0x04,0xce,0xef,0xb9,0xdc,0x2f] as IID_AsyncOperationCompletedHandler_1_Windows_Graphics_Imaging_BitmapPropertySet }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::CashDrawerEventSource, &'a ::rt::gen::windows::devices::pointofservice::CashDrawerClosedEventArgs> => [0x81495aa4,0x4476,0x577f,0xbf,0x88,0x7f,0xe8,0xa1,0x28,0x64,0x44] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_CashDrawerEventSource_Windows_Devices_PointOfService_CashDrawerClosedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::CashDrawerEventSource, &'a ::rt::gen::windows::devices::pointofservice::CashDrawerOpenedEventArgs> => [0x27648799,0xe7d5,0x5ce3,0x97,0xce,0x2f,0x41,0x10,0xdd,0x32,0x98] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_CashDrawerEventSource_Windows_Devices_PointOfService_CashDrawerOpenedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pointofservice::CashDrawer> => [0x45007467,0x92f2,0x5bff,0xb1,0x91,0xaa,0x50,0x00,0xfe,0xdd,0x9e] as IID_IAsyncOperation_1_Windows_Devices_PointOfService_CashDrawer }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pointofservice::CashDrawer> => [0x57836710,0xf186,0x5636,0x89,0x1d,0xf8,0xc5,0x39,0x8e,0xa6,0xdf] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_PointOfService_CashDrawer }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::pointofservice::ClaimedCashDrawer> => [0x9230e7aa,0x20a0,0x5752,0x9c,0x20,0x4b,0xf4,0x49,0x34,0xa8,0x7e] as IID_IAsyncOperation_1_Windows_Devices_PointOfService_ClaimedCashDrawer }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedCashDrawer> => [0xe68c3736,0xfde7,0x5cfb,0xb2,0x2f,0x92,0x11,0x97,0x23,0xe7,0x29] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_PointOfService_ClaimedCashDrawer }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::CashDrawer, &'a ::rt::gen::windows::devices::pointofservice::CashDrawerStatusUpdatedEventArgs> => [0x64662ef4,0xcb0e,0x5c6f,0xa8,0x20,0x7d,0x0a,0x76,0x95,0x54,0xc9] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_CashDrawer_Windows_Devices_PointOfService_CashDrawerStatusUpdatedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::CashDrawerCloseAlarm, &'a IInspectable> => [0xc54fbda4,0x5e0b,0x54c3,0x94,0xf2,0x83,0x35,0x1e,0x41,0xc4,0x6f] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_CashDrawerCloseAlarm_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::printers::extensions::Print3DWorkflow, &'a ::rt::gen::windows::devices::printers::extensions::Print3DWorkflowPrintRequestedEventArgs> => [0x5f4c6603,0x5512,0x59aa,0x8d,0x96,0xb1,0x38,0x9d,0x8b,0x57,0x96] as IID_TypedEventHandler_2_Windows_Devices_Printers_Extensions_Print3DWorkflow_Windows_Devices_Printers_Extensions_Print3DWorkflowPrintRequestedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::radios::Radio>> => [0x040b54a1,0x203e,0x58f5,0x94,0x3f,0xc1,0xcc,0xa8,0x6b,0xd5,0x32] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Radios_Radio }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::radios::Radio>> => [0xd30691e6,0x60a0,0x59c9,0x89,0x65,0x5b,0xbe,0x28,0x2e,0x82,0x08] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Radios_Radio }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::radios::Radio> => [0xeac62c40,0x8dbc,0x5854,0x8b,0xa0,0xb7,0xb9,0x94,0x0e,0x73,0x89] as IID_IAsyncOperation_1_Windows_Devices_Radios_Radio }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::radios::Radio> => [0x8a5c7e3a,0x80e2,0x585b,0x86,0x30,0x7a,0x8e,0x77,0x7f,0x03,0x54] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Radios_Radio }
		RT_PINTERFACE!{ for IAsyncOperation<::rt::gen::windows::devices::radios::RadioAccessStatus> => [0x21fb30ef,0x072f,0x502c,0x98,0x98,0xd0,0xc3,0xb2,0xcd,0x9a,0xc5] as IID_IAsyncOperation_1_Windows_Devices_Radios_RadioAccessStatus }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<::rt::gen::windows::devices::radios::RadioAccessStatus> => [0xbd248e73,0xf05f,0x574c,0xae,0x3d,0x9b,0x95,0xc4,0xbf,0x28,0x2a] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Radios_RadioAccessStatus }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::radios::Radio, &'a IInspectable> => [0xfc6aa329,0xb586,0x5ebb,0x9e,0x85,0x3f,0x6b,0x84,0xeb,0xdf,0x18] as IID_TypedEventHandler_2_Windows_Devices_Radios_Radio_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Accelerometer, &'a ::rt::gen::windows::devices::sensors::AccelerometerReadingChangedEventArgs> => [0xa5e83e40,0xb597,0x5b83,0x92,0xf5,0x5b,0xed,0x39,0x26,0xca,0x80] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Accelerometer_Windows_Devices_Sensors_AccelerometerReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Accelerometer, &'a ::rt::gen::windows::devices::sensors::AccelerometerShakenEventArgs> => [0x3e5d6eaf,0xf169,0x5d60,0x92,0xb0,0x98,0xcd,0x6b,0xd8,0xf8,0x08] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Accelerometer_Windows_Devices_Sensors_AccelerometerShakenEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Inclinometer, &'a ::rt::gen::windows::devices::sensors::InclinometerReadingChangedEventArgs> => [0x6f3b411f,0xd147,0x59f1,0xbb,0xe4,0x7b,0xec,0x39,0x6c,0x7b,0x6e] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Inclinometer_Windows_Devices_Sensors_InclinometerReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Gyrometer, &'a ::rt::gen::windows::devices::sensors::GyrometerReadingChangedEventArgs> => [0x15171524,0x5786,0x59a5,0xaf,0x5b,0xa0,0x12,0x45,0x72,0x6c,0x44] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Gyrometer_Windows_Devices_Sensors_GyrometerReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Compass, &'a ::rt::gen::windows::devices::sensors::CompassReadingChangedEventArgs> => [0xe787d73d,0xa121,0x5ae6,0xb4,0x97,0xab,0x93,0x48,0x37,0xe5,0x7f] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Compass_Windows_Devices_Sensors_CompassReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::LightSensor, &'a ::rt::gen::windows::devices::sensors::LightSensorReadingChangedEventArgs> => [0x1ecf183a,0x9f0a,0x5f73,0x92,0x25,0x5a,0x33,0xea,0xb5,0x59,0x4f] as IID_TypedEventHandler_2_Windows_Devices_Sensors_LightSensor_Windows_Devices_Sensors_LightSensorReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::OrientationSensor, &'a ::rt::gen::windows::devices::sensors::OrientationSensorReadingChangedEventArgs> => [0x91ae0c43,0xe1f7,0x577d,0xa1,0x61,0x8a,0xaf,0x27,0x5e,0xb9,0x27] as IID_TypedEventHandler_2_Windows_Devices_Sensors_OrientationSensor_Windows_Devices_Sensors_OrientationSensorReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::SimpleOrientationSensor, &'a ::rt::gen::windows::devices::sensors::SimpleOrientationSensorOrientationChangedEventArgs> => [0x92437fa7,0xea7b,0x5fc5,0x8e,0xcf,0x1b,0x91,0x1b,0xea,0x2b,0xfc] as IID_TypedEventHandler_2_Windows_Devices_Sensors_SimpleOrientationSensor_Windows_Devices_Sensors_SimpleOrientationSensorOrientationChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Magnetometer, &'a ::rt::gen::windows::devices::sensors::MagnetometerReadingChangedEventArgs> => [0xf9a0da76,0xc4fd,0x50ab,0x98,0xb6,0xbf,0xd2,0x6d,0x6d,0x3d,0x82] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Magnetometer_Windows_Devices_Sensors_MagnetometerReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::sensors::ActivitySensor> => [0xc33003ae,0xe7ae,0x572b,0x8d,0x55,0x7d,0xb1,0x97,0x35,0x6c,0x30] as IID_IAsyncOperation_1_Windows_Devices_Sensors_ActivitySensor }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::sensors::ActivitySensor> => [0xfb0594f4,0x93d9,0x5c2f,0xb8,0xeb,0x90,0xf1,0xe9,0x25,0x8f,0xdc] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Sensors_ActivitySensor }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReading>> => [0xcd781b82,0x7900,0x51a3,0x80,0xce,0x90,0x3e,0x2e,0x0a,0x4f,0x0e] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Sensors_ActivitySensorReading }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReading>> => [0x179fb953,0x2d58,0x5991,0x8f,0x5b,0xac,0x64,0x21,0x9a,0x11,0x01] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Sensors_ActivitySensorReading }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReading> => [0x79a87969,0x327f,0x5b7a,0xa0,0xd3,0x73,0xea,0xb1,0x6d,0xe2,0x1c] as IID_IAsyncOperation_1_Windows_Devices_Sensors_ActivitySensorReading }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReading> => [0xadc48d5d,0xb343,0x5a58,0x84,0x54,0x6e,0x2b,0xc2,0xe0,0x47,0x5c] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Sensors_ActivitySensorReading }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::ActivitySensor, &'a ::rt::gen::windows::devices::sensors::ActivitySensorReadingChangedEventArgs> => [0xa5b72e01,0x546c,0x5fbb,0xb8,0x47,0x49,0x20,0x0a,0xaa,0xaa,0xc5] as IID_TypedEventHandler_2_Windows_Devices_Sensors_ActivitySensor_Windows_Devices_Sensors_ActivitySensorReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Barometer, &'a ::rt::gen::windows::devices::sensors::BarometerReadingChangedEventArgs> => [0xe4caf459,0xd101,0x5ca5,0xa4,0xea,0xde,0xb0,0x91,0x7a,0xe2,0x7e] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Barometer_Windows_Devices_Sensors_BarometerReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::sensors::Pedometer> => [0x9414388f,0x1b3e,0x55f5,0x81,0x9b,0xab,0x38,0x33,0x64,0x60,0x55] as IID_IAsyncOperation_1_Windows_Devices_Sensors_Pedometer }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::sensors::Pedometer> => [0xa62dbe4e,0x51de,0x5a13,0xba,0x21,0xe7,0x6d,0xf3,0xbc,0x13,0x96] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Sensors_Pedometer }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::sensors::PedometerReading>> => [0x2aeac503,0xa3a8,0x57b3,0xa8,0xa9,0xe1,0x6b,0x0c,0xd4,0xc0,0xa4] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::sensors::PedometerReading>> => [0x5bbff840,0x59f2,0x5108,0x92,0x05,0xa0,0xbb,0xf8,0xf9,0xba,0x68] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Pedometer, &'a ::rt::gen::windows::devices::sensors::PedometerReadingChangedEventArgs> => [0xdcd47693,0xaad5,0x5b3c,0x9c,0x8d,0x14,0x0b,0x8b,0xc2,0x12,0x2b] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Pedometer_Windows_Devices_Sensors_PedometerReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::ProximitySensor, &'a ::rt::gen::windows::devices::sensors::ProximitySensorReadingChangedEventArgs> => [0x9f7e222b,0x892a,0x5e68,0xb0,0x8a,0x10,0x38,0x4b,0x5f,0x92,0xb9] as IID_TypedEventHandler_2_Windows_Devices_Sensors_ProximitySensor_Windows_Devices_Sensors_ProximitySensorReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::Altimeter, &'a ::rt::gen::windows::devices::sensors::AltimeterReadingChangedEventArgs> => [0xd775d699,0x9d74,0x5473,0x9c,0x1b,0xd5,0x1a,0x89,0xdb,0x66,0x42] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Altimeter_Windows_Devices_Sensors_AltimeterReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::sensors::custom::CustomSensor> => [0x7fbfbe55,0x9674,0x54e3,0xa2,0x69,0x9c,0xaa,0x82,0x0e,0xd2,0x3c] as IID_IAsyncOperation_1_Windows_Devices_Sensors_Custom_CustomSensor }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::sensors::custom::CustomSensor> => [0x808b62d7,0x6e02,0x5680,0xa5,0x9e,0x11,0x8a,0x98,0xa4,0xe7,0x0f] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Sensors_Custom_CustomSensor }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::sensors::custom::CustomSensor, &'a ::rt::gen::windows::devices::sensors::custom::CustomSensorReadingChangedEventArgs> => [0xaa9460cb,0xf08c,0x5963,0xb2,0x32,0xcc,0x40,0x75,0xe9,0x84,0xe7] as IID_TypedEventHandler_2_Windows_Devices_Sensors_Custom_CustomSensor_Windows_Devices_Sensors_Custom_CustomSensorReadingChangedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::serialcommunication::SerialDevice> => [0x44ef26ed,0xc1ff,0x546a,0xa4,0x6b,0x6a,0x37,0xde,0x91,0x87,0xfb] as IID_IAsyncOperation_1_Windows_Devices_SerialCommunication_SerialDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::serialcommunication::SerialDevice> => [0x84a34b33,0x06fc,0x5e63,0x8e,0xe2,0xea,0xb4,0xff,0x69,0xac,0xb7] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_SerialCommunication_SerialDevice }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::smartcards::SmartCardReader> => [0x036a830d,0xbbca,0x5cb9,0x97,0x7f,0xb2,0x9e,0xa3,0x04,0x21,0x49] as IID_IAsyncOperation_1_Windows_Devices_SmartCards_SmartCardReader }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::smartcards::SmartCardReader> => [0x20d3244d,0x375a,0x5f7d,0x89,0x44,0x16,0x4f,0xdf,0xed,0x42,0x39] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_SmartCards_SmartCardReader }
		RT_PINTERFACE!{ for IAsyncOperation<::rt::gen::windows::devices::smartcards::SmartCardReaderStatus> => [0x5ae402fa,0x1f22,0x5570,0xa0,0xc8,0xb2,0x32,0x0a,0xde,0xdb,0x81] as IID_IAsyncOperation_1_Windows_Devices_SmartCards_SmartCardReaderStatus }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<::rt::gen::windows::devices::smartcards::SmartCardReaderStatus> => [0x3d7e6ea9,0xe739,0x555c,0x9c,0x02,0x07,0x39,0x6c,0x53,0x21,0xf5] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_SmartCards_SmartCardReaderStatus }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::smartcards::SmartCard>> => [0x3b2691b2,0xfc5e,0x59ff,0x8c,0x6f,0xe6,0xdd,0x29,0xa9,0x67,0xfc] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_SmartCards_SmartCard }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::smartcards::SmartCard>> => [0xbfea3fad,0x411e,0x5721,0x88,0xf5,0x92,0xc9,0xb9,0xfb,0xbe,0x14] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_SmartCards_SmartCard }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::smartcards::SmartCardReader, &'a ::rt::gen::windows::devices::smartcards::CardAddedEventArgs> => [0xd36f2db9,0x5674,0x5f74,0x9f,0x69,0x3c,0xdc,0x45,0x59,0x99,0x9f] as IID_TypedEventHandler_2_Windows_Devices_SmartCards_SmartCardReader_Windows_Devices_SmartCards_CardAddedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::smartcards::SmartCardReader, &'a ::rt::gen::windows::devices::smartcards::CardRemovedEventArgs> => [0x69da07c6,0xb266,0x5a1c,0x93,0x7c,0xd8,0x2b,0x4a,0x82,0x32,0xc6] as IID_TypedEventHandler_2_Windows_Devices_SmartCards_SmartCardReader_Windows_Devices_SmartCards_CardRemovedEventArgs }
		RT_PINTERFACE!{ for IAsyncOperation<::rt::gen::windows::devices::smartcards::SmartCardStatus> => [0xe2223376,0x8cf6,0x51bd,0x99,0x07,0x13,0x44,0xaa,0x66,0x5e,0x5d] as IID_IAsyncOperation_1_Windows_Devices_SmartCards_SmartCardStatus }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<::rt::gen::windows::devices::smartcards::SmartCardStatus> => [0xbdaf4a41,0x3b4a,0x56b0,0xae,0xec,0xfe,0xe7,0x1c,0xc7,0xf3,0x28] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_SmartCards_SmartCardStatus }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::smartcards::SmartCardProvisioning> => [0x6184fc80,0xb752,0x5ce8,0xa1,0x36,0xf5,0x71,0x74,0xbb,0x93,0x09] as IID_IAsyncOperation_1_Windows_Devices_SmartCards_SmartCardProvisioning }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::smartcards::SmartCardProvisioning> => [0x7a2e58dc,0x22ee,0x5cb8,0x83,0xcc,0xa7,0xa6,0x1b,0x9d,0xcd,0x2c] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_SmartCards_SmartCardProvisioning }
		RT_PINTERFACE!{ for IAsyncOperation<::Guid> => [0x6607bc41,0x294b,0x5975,0x9c,0x3f,0x4b,0x49,0x83,0x6d,0x09,0x16] as IID_IAsyncOperation_1_System_Guid }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<::Guid> => [0x5233899b,0xba7e,0x504f,0xbb,0x83,0xce,0xeb,0xac,0x62,0xde,0xcf] as IID_AsyncOperationCompletedHandler_1_System_Guid }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::smartcards::SmartCardChallengeContext> => [0x1c650663,0x3f68,0x599b,0xb9,0xd4,0xc3,0x50,0xf1,0x3e,0xe4,0xe4] as IID_IAsyncOperation_1_Windows_Devices_SmartCards_SmartCardChallengeContext }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::smartcards::SmartCardChallengeContext> => [0x96b172f6,0xdedb,0x5f3e,0xaf,0x90,0x7b,0x0f,0x10,0x21,0x93,0x52] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_SmartCards_SmartCardChallengeContext }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::smartcards::SmartCardConnection> => [0x779bbc5b,0xa75c,0x5988,0x97,0x8f,0x34,0xdb,0xc6,0x29,0xd5,0x76] as IID_IAsyncOperation_1_Windows_Devices_SmartCards_SmartCardConnection }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::smartcards::SmartCardConnection> => [0xc71f00e6,0xaf26,0x5e5c,0x91,0x3d,0x0e,0xfe,0xb7,0xd0,0x8e,0xf7] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_SmartCards_SmartCardConnection }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::usb::UsbDevice> => [0x2138c5ed,0xb71a,0x5166,0x99,0x48,0xd5,0x57,0x92,0x74,0x8f,0x5c] as IID_IAsyncOperation_1_Windows_Devices_Usb_UsbDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::usb::UsbDevice> => [0x7331254f,0x6caf,0x587d,0x9c,0x2a,0x01,0x8c,0x66,0xd3,0x12,0xdb] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Usb_UsbDevice }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::usb::UsbInterruptInPipe, &'a ::rt::gen::windows::devices::usb::UsbInterruptInEventArgs> => [0xe6db9449,0xf36a,0x50f2,0x92,0x6c,0x2a,0xfd,0x85,0xc4,0x9f,0x01] as IID_TypedEventHandler_2_Windows_Devices_Usb_UsbInterruptInPipe_Windows_Devices_Usb_UsbInterruptInEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::wifi::WiFiAdapter>> => [0x3140802b,0x987c,0x5c56,0xa4,0x30,0x90,0xfb,0xc1,0x89,0x8d,0xda] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_WiFi_WiFiAdapter }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::wifi::WiFiAdapter>> => [0x92902a07,0x2f18,0x56e9,0x87,0xfb,0x24,0xfe,0x19,0xf7,0x06,0x88] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_WiFi_WiFiAdapter }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::wifi::WiFiAdapter> => [0x1dcf739d,0x10b7,0x59e9,0xab,0x47,0x8b,0x02,0x77,0xe2,0x01,0x93] as IID_IAsyncOperation_1_Windows_Devices_WiFi_WiFiAdapter }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::wifi::WiFiAdapter> => [0x35362f75,0x6aae,0x560d,0xb3,0xd0,0x3f,0xae,0x9c,0x72,0x60,0xa8] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_WiFi_WiFiAdapter }
		RT_PINTERFACE!{ for IAsyncOperation<::rt::gen::windows::devices::wifi::WiFiAccessStatus> => [0xf8c75a3a,0x739a,0x57aa,0x98,0x6d,0x1f,0x76,0x04,0xd7,0xe3,0x86] as IID_IAsyncOperation_1_Windows_Devices_WiFi_WiFiAccessStatus }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<::rt::gen::windows::devices::wifi::WiFiAccessStatus> => [0x65e889ca,0xf6c9,0x5c75,0xbe,0xf9,0x05,0xab,0x88,0xa4,0x9a,0x54] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_WiFi_WiFiAccessStatus }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifi::WiFiAdapter, &'a IInspectable> => [0xf6c02d1b,0x43e8,0x5fc8,0x8e,0x8e,0xee,0x7b,0x80,0x94,0xb6,0x83] as IID_TypedEventHandler_2_Windows_Devices_WiFi_WiFiAdapter_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::wifi::WiFiConnectionResult> => [0xffa41f49,0x4c30,0x50d3,0x95,0x49,0xe4,0xf0,0x55,0xb4,0x17,0xb4] as IID_IAsyncOperation_1_Windows_Devices_WiFi_WiFiConnectionResult }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::wifi::WiFiConnectionResult> => [0xf380eb8d,0x1e52,0x5350,0xa2,0x88,0x86,0x1c,0x96,0x3a,0x84,0xf0] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_WiFi_WiFiConnectionResult }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectDevice> => [0xdad01b61,0xa82d,0x566c,0xba,0x82,0x22,0x4c,0x11,0x50,0x06,0x69] as IID_IAsyncOperation_1_Windows_Devices_WiFiDirect_WiFiDirectDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectDevice> => [0xd34abe17,0xfb19,0x57be,0xbc,0x41,0x0e,0xb8,0x3d,0xea,0x15,0x1c] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_WiFiDirect_WiFiDirectDevice }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisementPublisher, &'a ::rt::gen::windows::devices::wifidirect::WiFiDirectAdvertisementPublisherStatusChangedEventArgs> => [0xde73cba7,0x370d,0x550c,0xb2,0x3a,0x53,0xdd,0x0b,0x4e,0x48,0x0d] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_WiFiDirectAdvertisementPublisher_Windows_Devices_WiFiDirect_WiFiDirectAdvertisementPublisherStatusChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectConnectionListener, &'a ::rt::gen::windows::devices::wifidirect::WiFiDirectConnectionRequestedEventArgs> => [0xd04b0403,0x1fe2,0x532f,0x8e,0x47,0x48,0x23,0xa1,0x4e,0x62,0x4f] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_WiFiDirectConnectionListener_Windows_Devices_WiFiDirect_WiFiDirectConnectionRequestedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAdvertiser, &'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSessionRequestedEventArgs> => [0xcb98fd74,0x871d,0x5730,0x91,0xfe,0x81,0xef,0x94,0x7f,0xe7,0x8f] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceAdvertiser_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceSessionRequestedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAdvertiser, &'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAutoAcceptSessionConnectedEventArgs> => [0x3be2d508,0xa856,0x5c09,0x99,0x98,0x52,0x25,0x97,0xb4,0x4b,0x07] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceAdvertiser_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceAutoAcceptSessionConnectedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceAdvertiser, &'a IInspectable> => [0x67fc3121,0xc1a0,0x5c23,0xaf,0x58,0xec,0xb7,0xf2,0xa7,0xd7,0x73] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceAdvertiser_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession> => [0xc2da4e97,0x728b,0x5401,0xa9,0xd9,0x3a,0x01,0x85,0x45,0x0a,0xf2] as IID_IAsyncOperation_1_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceSession }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession> => [0xb29de711,0x60b8,0x59da,0x8f,0x4d,0xfc,0x79,0xd8,0xcc,0xd4,0x22] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceSession }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectService> => [0xc4fa2ae8,0x4ff7,0x5aa0,0xaf,0x97,0xed,0x85,0xea,0x66,0xf9,0xae] as IID_IAsyncOperation_1_Windows_Devices_WiFiDirect_Services_WiFiDirectService }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectService> => [0xf505a3c8,0x4837,0x5e0e,0x8a,0x4d,0x1e,0x2a,0xf5,0x47,0x7e,0x5c] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_WiFiDirect_Services_WiFiDirectService }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectService, &'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSessionDeferredEventArgs> => [0xfc3dfc2c,0x9cfa,0x5822,0xba,0x3f,0xff,0x3a,0xfb,0x65,0x77,0x7e] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_Services_WiFiDirectService_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceSessionDeferredEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceProvisioningInfo> => [0xd7fa4dc4,0x4730,0x506e,0xbf,0xf0,0x80,0x1e,0xb4,0xa8,0x31,0xa8] as IID_IAsyncOperation_1_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceProvisioningInfo }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceProvisioningInfo> => [0x94cb9568,0x040a,0x5186,0xa3,0xc9,0x52,0x68,0x0e,0xe1,0x79,0x84] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceProvisioningInfo }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidInputReport> => [0xb3e28917,0xcd48,0x57b3,0xa0,0xb1,0x32,0x14,0x32,0xe8,0x5b,0xd6] as IID_IAsyncOperation_1_Windows_Devices_HumanInterfaceDevice_HidInputReport }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidInputReport> => [0x01c83770,0x03ab,0x5576,0x98,0xb4,0x8d,0x75,0xce,0x1a,0x98,0x85] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_HumanInterfaceDevice_HidInputReport }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidFeatureReport> => [0xd72fb6f9,0x42f6,0x5f45,0xbf,0xe3,0x29,0xaf,0x24,0x7c,0x2e,0x85] as IID_IAsyncOperation_1_Windows_Devices_HumanInterfaceDevice_HidFeatureReport }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidFeatureReport> => [0xdb643555,0x3d16,0x57fe,0xb7,0xef,0x2b,0xdb,0xd7,0x19,0xfd,0xbf] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_HumanInterfaceDevice_HidFeatureReport }
		RT_PINTERFACE!{ for IAsyncOperation<u32> => [0xef60385f,0xbe78,0x584b,0xaa,0xef,0x78,0x29,0xad,0xa2,0xb0,0xde] as IID_IAsyncOperation_1_System_UInt32 }
		RT_PINTERFACE!{ for AsyncOperationCompletedHandler<u32> => [0x9343b6e7,0xe3d2,0x5e4a,0xab,0x2d,0x2b,0xce,0x49,0x19,0xa6,0xa4] as IID_AsyncOperationCompletedHandler_1_System_UInt32 }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidDevice, &'a ::rt::gen::windows::devices::humaninterfacedevice::HidInputReportReceivedEventArgs> => [0x31e757c8,0x8f6a,0x540b,0x93,0x8b,0xab,0xa7,0x9b,0x6f,0x03,0xec] as IID_TypedEventHandler_2_Windows_Devices_HumanInterfaceDevice_HidDevice_Windows_Devices_HumanInterfaceDevice_HidInputReportReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::lights::Lamp, &'a ::rt::gen::windows::devices::lights::LampAvailabilityChangedEventArgs> => [0x556a02d9,0x7685,0x576f,0x89,0xca,0xb6,0x2d,0xc4,0x81,0xd2,0x9d] as IID_TypedEventHandler_2_Windows_Devices_Lights_Lamp_Windows_Devices_Lights_LampAvailabilityChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::midi::MidiInPort, &'a ::rt::gen::windows::devices::midi::MidiMessageReceivedEventArgs> => [0x50017240,0xcc39,0x5775,0x8a,0x6b,0xf6,0xf2,0x23,0x86,0xbf,0xca] as IID_TypedEventHandler_2_Windows_Devices_Midi_MidiInPort_Windows_Devices_Midi_MidiMessageReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionControlSession, &'a IInspectable> => [0xabc21152,0x2495,0x5e8c,0xae,0xd3,0x7d,0xc5,0x53,0x09,0xac,0x08] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionControlSession_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameReader, &'a ::rt::gen::windows::devices::perception::PerceptionColorFrameArrivedEventArgs> => [0xa4a50ea5,0x778d,0x5056,0xa1,0xcf,0x54,0x6a,0x1b,0xe2,0xc0,0x10] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionColorFrameReader_Windows_Devices_Perception_PerceptionColorFrameArrivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameReader, &'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameArrivedEventArgs> => [0x4d529b7e,0xeea0,0x511b,0x82,0x85,0x47,0xe8,0xc8,0x5d,0x02,0x95] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionDepthFrameReader_Windows_Devices_Perception_PerceptionDepthFrameArrivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameReader, &'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameArrivedEventArgs> => [0x687fef67,0x8871,0x56fe,0x8e,0x7e,0x1d,0x29,0x29,0xcc,0x6f,0x42] as IID_TypedEventHandler_2_Windows_Devices_Perception_PerceptionInfraredFrameReader_Windows_Devices_Perception_PerceptionInfraredFrameArrivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner, &'a ::rt::gen::windows::devices::pointofservice::BarcodeScannerDataReceivedEventArgs> => [0x4f64e49a,0xbd8c,0x549d,0x97,0x0c,0xa5,0xa2,0x50,0xbd,0x27,0xca] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedBarcodeScanner_Windows_Devices_PointOfService_BarcodeScannerDataReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> EventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner> => [0xc0acd2ae,0x5b55,0x588d,0x81,0x1b,0xbe,0x33,0xa4,0xfd,0xda,0x57] as IID_EventHandler_1_Windows_Devices_PointOfService_ClaimedBarcodeScanner }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner, &'a ::rt::gen::windows::devices::pointofservice::BarcodeScannerImagePreviewReceivedEventArgs> => [0xfba116af,0x2a39,0x516f,0xa5,0x79,0xcc,0x3e,0xaf,0x36,0xa3,0x4b] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedBarcodeScanner_Windows_Devices_PointOfService_BarcodeScannerImagePreviewReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedBarcodeScanner, &'a ::rt::gen::windows::devices::pointofservice::BarcodeScannerErrorOccurredEventArgs> => [0xba42ff49,0xde12,0x5406,0x97,0x9e,0x06,0xc4,0x5c,0xa2,0xd5,0xa4] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedBarcodeScanner_Windows_Devices_PointOfService_BarcodeScannerErrorOccurredEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader, &'a ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderBankCardDataReceivedEventArgs> => [0x6a41d015,0x245e,0x51ba,0xbd,0x6c,0x44,0x77,0x5d,0x70,0xbf,0xa2] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedMagneticStripeReader_Windows_Devices_PointOfService_MagneticStripeReaderBankCardDataReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader, &'a ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderAamvaCardDataReceivedEventArgs> => [0x29e08f92,0xc3ab,0x57ea,0xaa,0xba,0x78,0x9f,0x79,0x2d,0x7a,0x46] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedMagneticStripeReader_Windows_Devices_PointOfService_MagneticStripeReaderAamvaCardDataReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader, &'a ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderVendorSpecificCardDataReceivedEventArgs> => [0x959124ba,0xee44,0x560c,0x81,0x83,0x61,0xc6,0xa2,0x30,0x8d,0x8f] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedMagneticStripeReader_Windows_Devices_PointOfService_MagneticStripeReaderVendorSpecificCardDataReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> EventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader> => [0x8006bf5d,0x0895,0x5b8c,0x80,0x8a,0x6b,0xb8,0xf2,0x67,0x94,0xfa] as IID_EventHandler_1_Windows_Devices_PointOfService_ClaimedMagneticStripeReader }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedMagneticStripeReader, &'a ::rt::gen::windows::devices::pointofservice::MagneticStripeReaderErrorOccurredEventArgs> => [0x1464a1e6,0x9d92,0x547e,0xb4,0xac,0xf2,0x55,0xac,0x85,0xf9,0x50] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedMagneticStripeReader_Windows_Devices_PointOfService_MagneticStripeReaderErrorOccurredEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedPosPrinter, &'a ::rt::gen::windows::devices::pointofservice::PosPrinterReleaseDeviceRequestedEventArgs> => [0x31424f6f,0xcfeb,0x5031,0x8a,0x95,0xbe,0xa5,0x9b,0x09,0xe5,0x84] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedPosPrinter_Windows_Devices_PointOfService_PosPrinterReleaseDeviceRequestedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::pointofservice::ClaimedCashDrawer, &'a IInspectable> => [0xdb886581,0x2462,0x5c81,0x88,0x0c,0x06,0x11,0x2c,0xa7,0x00,0x12] as IID_TypedEventHandler_2_Windows_Devices_PointOfService_ClaimedCashDrawer_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::serialcommunication::SerialDevice, &'a ::rt::gen::windows::devices::serialcommunication::ErrorReceivedEventArgs> => [0xd92ea323,0xb7bf,0x5e02,0xb9,0xfb,0xc6,0x1f,0x97,0xd0,0x80,0xe9] as IID_TypedEventHandler_2_Windows_Devices_SerialCommunication_SerialDevice_Windows_Devices_SerialCommunication_ErrorReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::serialcommunication::SerialDevice, &'a ::rt::gen::windows::devices::serialcommunication::PinChangedEventArgs> => [0xe496c3ef,0x5802,0x5ac4,0xac,0x2e,0x96,0xbc,0x23,0xfa,0x94,0x47] as IID_TypedEventHandler_2_Windows_Devices_SerialCommunication_SerialDevice_Windows_Devices_SerialCommunication_PinChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectDevice, &'a IInspectable> => [0x9208929a,0x2a3c,0x50ad,0xaa,0x08,0xa0,0xa9,0x86,0xed,0xba,0xbe] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_WiFiDirectDevice_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession, &'a IInspectable> => [0x10c33301,0xe31c,0x5cce,0xb2,0xa0,0xc1,0xdc,0x2d,0x8d,0x0e,0x13] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceSession_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceSession, &'a ::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceRemotePortAddedEventArgs> => [0x8326a337,0x3c19,0x57a7,0x80,0xec,0xcc,0xa2,0xea,0x62,0xef,0x12] as IID_TypedEventHandler_2_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceSession_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceRemotePortAddedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::streams::IOutputStream> => [0xe8736833,0xd013,0x5361,0x97,0x7d,0xc5,0xe9,0x99,0x34,0x68,0x0e] as IID_IAsyncOperation_1_Windows_Storage_Streams_IOutputStream }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::streams::IOutputStream> => [0xbcb37f4f,0x3af4,0x561c,0xa9,0xe3,0xee,0xf1,0x73,0x84,0x94,0xd7] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_Streams_IOutputStream }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::networking::sockets::DatagramSocket, &'a ::rt::gen::windows::networking::sockets::DatagramSocketMessageReceivedEventArgs> => [0x4482e19b,0x2389,0x5767,0x9b,0x0b,0x8d,0x7a,0x8e,0xf5,0x57,0x43] as IID_TypedEventHandler_2_Windows_Networking_Sockets_DatagramSocket_Windows_Networking_Sockets_DatagramSocketMessageReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::custom::CustomDevice> => [0x2a6344aa,0x0568,0x548e,0xa1,0xa2,0xb6,0xbb,0x45,0x1d,0x22,0x8c] as IID_IAsyncOperation_1_Windows_Devices_Custom_CustomDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::custom::CustomDevice> => [0x1fdd39b0,0xe0e5,0x5c59,0xb2,0x7d,0xa5,0x49,0xb1,0x07,0x5c,0xe9] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Custom_CustomDevice }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::sms::ISmsMessage> => [0xabf9b459,0x48c3,0x5eac,0x97,0x49,0x4c,0x6d,0xb4,0xd5,0x07,0xe6] as IID_IAsyncOperation_1_Windows_Devices_Sms_ISmsMessage }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::sms::ISmsMessage> => [0x4e6c4c86,0xebe6,0x55d9,0xad,0xc0,0xfe,0xcc,0x38,0xc8,0x2a,0xa2] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Sms_ISmsMessage }
		RT_PINTERFACE!{ for<'a> IAsyncOperationWithProgress<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::sms::ISmsMessage>, i32> => [0x12f85589,0x415d,0x5b5d,0xb0,0xd0,0xfd,0xa3,0xb0,0x29,0x5a,0xdc] as IID_IAsyncOperationWithProgress_2_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Sms_ISmsMessage__System_Int32 }
		RT_PINTERFACE!{ for<'a> AsyncOperationProgressHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::sms::ISmsMessage>, i32> => [0x3f9d1255,0xebf8,0x569f,0x91,0xc3,0x49,0x74,0x0d,0x59,0x44,0xce] as IID_AsyncOperationProgressHandler_2_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Sms_ISmsMessage__System_Int32 }
		RT_PINTERFACE!{ for<'a> AsyncOperationWithProgressCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::devices::sms::ISmsMessage>, i32> => [0xc0454cfc,0x2f2f,0x5e0c,0x8d,0xe9,0x58,0xb9,0xe8,0x2a,0x03,0xba] as IID_AsyncOperationWithProgressCompletedHandler_2_Windows_Foundation_Collections_IVectorView_1_Windows_Devices_Sms_ISmsMessage__System_Int32 }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::sms::SmsDevice> => [0xab710de1,0xfcbb,0x5bd6,0x9f,0x2f,0x28,0x5f,0xa9,0xfb,0x44,0xe8] as IID_IAsyncOperation_1_Windows_Devices_Sms_SmsDevice }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::sms::SmsDevice> => [0x44aa5484,0x43d0,0x5893,0xa4,0xee,0x7d,0xb0,0x01,0x13,0xae,0x60] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Sms_SmsDevice }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::StorageFile> => [0x5e52f8ce,0xaced,0x5a42,0x95,0xb4,0xf6,0x74,0xdd,0x84,0x88,0x5e] as IID_IAsyncOperation_1_Windows_Storage_StorageFile }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::StorageFile> => [0xe521c894,0x2c26,0x5946,0x9e,0x61,0x2b,0x5e,0x18,0x8d,0x01,0xed] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_StorageFile }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::StorageFolder> => [0x6be9e7d7,0xe83a,0x5cbc,0x80,0x2c,0x17,0x68,0x96,0x0b,0x52,0xc3] as IID_IAsyncOperation_1_Windows_Storage_StorageFolder }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::StorageFolder> => [0xc211026e,0x9e63,0x5452,0xba,0x54,0x3a,0x07,0xd6,0xa9,0x68,0x74] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_StorageFolder }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::IStorageItem> => [0x5fc9c137,0xebb7,0x5e6c,0x9c,0xba,0x68,0x6f,0x2e,0xc2,0xb0,0xbb] as IID_IAsyncOperation_1_Windows_Storage_IStorageItem }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::IStorageItem> => [0x92c3102f,0xa327,0x5318,0xa6,0xc1,0x76,0xf6,0xb2,0xa0,0xab,0xfb] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_IStorageItem }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::storage::StorageFile>> => [0x03362e33,0xe413,0x5f29,0x97,0xd0,0x48,0xa4,0x78,0x09,0x35,0xf9] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Storage_StorageFile }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::storage::StorageFile>> => [0xcb4206c5,0x0988,0x5104,0xaf,0xa9,0x25,0x3c,0x29,0x8f,0x86,0xfd] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Storage_StorageFile }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::storage::StorageFolder>> => [0xca40b21b,0xaeb1,0x5a61,0x9e,0x08,0x3b,0xd5,0xd9,0x59,0x40,0x23] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Storage_StorageFolder }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::storage::StorageFolder>> => [0xed2d1d9b,0x26ec,0x5be7,0xa8,0xa3,0x56,0x45,0x89,0x33,0xd2,0x5f] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Storage_StorageFolder }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::storage::IStorageItem>> => [0x4b1c0fd7,0x7a01,0x5e7a,0xa6,0xfe,0xbe,0x45,0x00,0x28,0x3f,0x23] as IID_IAsyncOperation_1_Windows_Foundation_Collections_IVectorView_1_Windows_Storage_IStorageItem }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::collections::IVectorView<&'a ::rt::gen::windows::storage::IStorageItem>> => [0x51436e75,0xace1,0x5a68,0xb2,0x60,0xf8,0x43,0xb8,0x46,0xf0,0xdb] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Collections_IVectorView_1_Windows_Storage_IStorageItem }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::fileproperties::BasicProperties> => [0x5186131a,0x4467,0x504b,0x97,0x7a,0x07,0x85,0xa8,0x23,0x04,0x85] as IID_IAsyncOperation_1_Windows_Storage_FileProperties_BasicProperties }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::fileproperties::BasicProperties> => [0xc8659aae,0x4926,0x52ad,0x8f,0x60,0xd8,0x9f,0xe5,0xa8,0xdf,0x5f] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_FileProperties_BasicProperties }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::streams::IRandomAccessStream> => [0x430ecece,0x1418,0x5d19,0x81,0xb2,0x5d,0xdb,0x38,0x16,0x03,0xcc] as IID_IAsyncOperation_1_Windows_Storage_Streams_IRandomAccessStream }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::streams::IRandomAccessStream> => [0x398c4183,0x793d,0x5b00,0x81,0x9b,0x4a,0xef,0x92,0x48,0x5e,0x94] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_Streams_IRandomAccessStream }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::storage::StorageStreamTransaction> => [0x0d81405a,0x9bd3,0x5e87,0x82,0xf4,0x9b,0x41,0x28,0xa8,0x87,0xeb] as IID_IAsyncOperation_1_Windows_Storage_StorageStreamTransaction }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::storage::StorageStreamTransaction> => [0xd11739e6,0x2995,0x5d33,0xbf,0xff,0x51,0xb6,0x04,0x1f,0x68,0xc1] as IID_AsyncOperationCompletedHandler_1_Windows_Storage_StorageStreamTransaction }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::scanners::ImageScannerPreviewResult> => [0x2f74576f,0x0498,0x5348,0xbc,0x3b,0xa7,0x0d,0x1a,0x77,0x17,0x18] as IID_IAsyncOperation_1_Windows_Devices_Scanners_ImageScannerPreviewResult }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::scanners::ImageScannerPreviewResult> => [0xc054a410,0xac3c,0x5353,0xb1,0xee,0xe8,0x5e,0x78,0xfa,0xf3,0xf1] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Scanners_ImageScannerPreviewResult }
		RT_PINTERFACE!{ for<'a> IAsyncOperationWithProgress<&'a ::rt::gen::windows::devices::scanners::ImageScannerScanResult, u32> => [0x6e6e228a,0xf618,0x5d33,0x85,0x23,0x02,0xd1,0x66,0x72,0x66,0x5b] as IID_IAsyncOperationWithProgress_2_Windows_Devices_Scanners_ImageScannerScanResult_System_UInt32 }
		RT_PINTERFACE!{ for<'a> AsyncOperationProgressHandler<&'a ::rt::gen::windows::devices::scanners::ImageScannerScanResult, u32> => [0xd1662baa,0x4f20,0x5d18,0x97,0xf1,0xa0,0x1a,0x6d,0x0d,0xd9,0x80] as IID_AsyncOperationProgressHandler_2_Windows_Devices_Scanners_ImageScannerScanResult_System_UInt32 }
		RT_PINTERFACE!{ for<'a> AsyncOperationWithProgressCompletedHandler<&'a ::rt::gen::windows::devices::scanners::ImageScannerScanResult, u32> => [0xbd8bdbd8,0x459a,0x52dc,0xb1,0x01,0x75,0xb3,0x98,0xa6,0x1a,0xef] as IID_AsyncOperationWithProgressCompletedHandler_2_Windows_Devices_Scanners_ImageScannerScanResult_System_UInt32 }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::devices::scanners::ImageScanner> => [0x75d78736,0x6c52,0x551e,0xab,0x5f,0x50,0x67,0x4f,0x32,0x34,0x31] as IID_IAsyncOperation_1_Windows_Devices_Scanners_ImageScanner }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::devices::scanners::ImageScanner> => [0xb35ad6b4,0x0da0,0x5241,0x87,0xff,0xee,0xf3,0xa1,0x88,0x32,0x43] as IID_AsyncOperationCompletedHandler_1_Windows_Devices_Scanners_ImageScanner }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::devices::printers::extensions::PrintTaskConfiguration, &'a ::rt::gen::windows::devices::printers::extensions::PrintTaskConfigurationSaveRequestedEventArgs> => [0x0363f57a,0xb7a2,0x5e20,0xa1,0x56,0x25,0x34,0x23,0xe7,0xee,0x40] as IID_TypedEventHandler_2_Windows_Devices_Printers_Extensions_PrintTaskConfiguration_Windows_Devices_Printers_Extensions_PrintTaskConfigurationSaveRequestedEventArgs }
		RT_PINTERFACE!{ for<'a> EventHandler<&'a ::rt::gen::windows::foundation::diagnostics::TracingStatusChangedEventArgs> => [0x2bf27008,0x2eb4,0x5675,0xb1,0xcd,0xe9,0x90,0x6c,0xc5,0xce,0x64] as IID_EventHandler_1_Windows_Foundation_Diagnostics_TracingStatusChangedEventArgs }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::foundation::IMemoryBufferReference, &'a IInspectable> => [0xf4637d4a,0x0760,0x5431,0xbf,0xc0,0x24,0xeb,0x1d,0x4f,0x6c,0x4f] as IID_TypedEventHandler_2_Windows_Foundation_IMemoryBufferReference_System_Object }
		RT_PINTERFACE!{ for<'a> IAsyncOperation<&'a ::rt::gen::windows::foundation::diagnostics::ErrorDetails> => [0x9b05106d,0x77e0,0x5c24,0x82,0xb0,0x9b,0x2d,0xc8,0xf7,0x96,0x71] as IID_IAsyncOperation_1_Windows_Foundation_Diagnostics_ErrorDetails }
		RT_PINTERFACE!{ for<'a> AsyncOperationCompletedHandler<&'a ::rt::gen::windows::foundation::diagnostics::ErrorDetails> => [0xa6997f9d,0x7195,0x5972,0x8e,0xcd,0x1c,0x73,0xaa,0x5c,0xb3,0x12] as IID_AsyncOperationCompletedHandler_1_Windows_Foundation_Diagnostics_ErrorDetails }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::foundation::diagnostics::ILoggingChannel, &'a IInspectable> => [0x52c9c2a1,0x54a3,0x5ef9,0x9a,0xff,0x01,0x4e,0x7c,0x45,0x46,0x55] as IID_TypedEventHandler_2_Windows_Foundation_Diagnostics_ILoggingChannel_System_Object }
		RT_PINTERFACE!{ for<'a> TypedEventHandler<&'a ::rt::gen::windows::foundation::diagnostics::IFileLoggingSession, &'a ::rt::gen::windows::foundation::diagnostics::LogFileGeneratedEventArgs> => [0x0c6563b0,0x9d8b,0x5b60,0x99,0x4b,0xde,0xe1,0x17,0x4d,0x1e,0xfb] as IID_TypedEventHandler_2_Windows_Foundation_Diagnostics_IFileLoggingSession_Windows_Foundation_Diagnostics_LogFileGeneratedEventArgs }
pub mod collections { // Windows.Foundation.Collections
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum CollectionChange: i32 {
			Reset (CollectionChange_Reset) = 0, ItemInserted (CollectionChange_ItemInserted) = 1, ItemRemoved (CollectionChange_ItemRemoved) = 2, ItemChanged (CollectionChange_ItemChanged) = 3,
		}}
		DEFINE_IID!(IID_IVectorChangedEventArgs, 1465463775, 13566, 17536, 175, 21, 7, 105, 31, 61, 93, 155);
		RT_INTERFACE!{interface IVectorChangedEventArgs(IVectorChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IVectorChangedEventArgs] {
			fn get_CollectionChange(&mut self, out: *mut ::rt::gen::windows::foundation::collections::CollectionChange) -> ::w::HRESULT,
			fn get_Index(&mut self, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IPropertySet, 2319707551, 62694, 17441, 172, 249, 29, 171, 41, 134, 130, 12);
		RT_INTERFACE!{interface IPropertySet(IPropertySetVtbl): IInspectable(IInspectableVtbl) [IID_IPropertySet] {
			
		}}
		RT_CLASS!(PropertySet: ::rt::gen::windows::foundation::collections::IPropertySet);
		RT_CLASS!(ValueSet: ::rt::gen::windows::foundation::collections::IPropertySet);
		RT_CLASS!(StringMap: ::rt::gen::windows::foundation::collections::IMap<&'static str, &'static str>);
		DEFINE_IID!(IID_IIterable, 4205151722, 25108, 16919, 175, 218, 127, 70, 222, 88, 105, 179);
		RT_INTERFACE!{interface IIterable<T>(IIterableVtbl): IInspectable(IInspectableVtbl) [IID_IIterable] {
			fn First(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IIterator<T>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IIterator, 1786374243, 17152, 17818, 153, 102, 203, 182, 96, 150, 62, 225);
		RT_INTERFACE!{interface IIterator<T>(IIteratorVtbl): IInspectable(IInspectableVtbl) [IID_IIterator] {
			fn get_Current(&mut self, out: *mut T::Abi) -> ::w::HRESULT,
			fn get_HasCurrent(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn MoveNext(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn GetMany(&mut self, items: *mut T::Abi, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IVectorView, 3152149068, 45283, 17795, 186, 239, 31, 27, 46, 72, 62, 86);
		RT_INTERFACE!{interface IVectorView<T>(IVectorViewVtbl): IInspectable(IInspectableVtbl) [IID_IVectorView] {
			fn GetAt(&mut self, index: u32, out: *mut T::Abi) -> ::w::HRESULT,
			fn get_Size(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn IndexOf(&mut self, value: T::Abi, index: *mut u32, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn GetMany(&mut self, startIndex: u32, items: *mut T::Abi, out: *mut u32) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IVector, 2436052969, 4513, 17221, 163, 162, 78, 127, 149, 110, 34, 45);
		RT_INTERFACE!{interface IVector<T>(IVectorVtbl): IInspectable(IInspectableVtbl) [IID_IVector] {
			fn GetAt(&mut self, index: u32, out: *mut T::Abi) -> ::w::HRESULT,
			fn get_Size(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn GetView(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IVectorView<T>) -> ::w::HRESULT,
			fn IndexOf(&mut self, value: T::Abi, index: *mut u32, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn SetAt(&mut self, index: u32, value: T::Abi) -> ::w::HRESULT,
			fn InsertAt(&mut self, index: u32, value: T::Abi) -> ::w::HRESULT,
			fn RemoveAt(&mut self, index: u32) -> ::w::HRESULT,
			fn Append(&mut self, value: T::Abi) -> ::w::HRESULT,
			fn RemoveAtEnd(&mut self) -> ::w::HRESULT,
			fn Clear(&mut self) -> ::w::HRESULT,
			fn GetMany(&mut self, startIndex: u32, items: *mut T::Abi, out: *mut u32) -> ::w::HRESULT,
			fn ReplaceAll(&mut self, items: *mut T::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IKeyValuePair, 45422889, 49604, 19070, 137, 64, 3, 18, 181, 193, 133, 0);
		RT_INTERFACE!{interface IKeyValuePair<K, V>(IKeyValuePairVtbl): IInspectable(IInspectableVtbl) [IID_IKeyValuePair] {
			fn get_Key(&mut self, out: *mut K::Abi) -> ::w::HRESULT,
			fn get_Value(&mut self, out: *mut V::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMap, 1009329662, 34073, 17857, 170, 121, 25, 123, 103, 24, 193, 193);
		RT_INTERFACE!{interface IMap<K, V>(IMapVtbl): IInspectable(IInspectableVtbl) [IID_IMap] {
			fn Lookup(&mut self, key: K::Abi, out: *mut V::Abi) -> ::w::HRESULT,
			fn get_Size(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn HasKey(&mut self, key: K::Abi, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn GetView(&mut self, out: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<K, V>) -> ::w::HRESULT,
			fn Insert(&mut self, key: K::Abi, value: V::Abi, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn Remove(&mut self, key: K::Abi) -> ::w::HRESULT,
			fn Clear(&mut self) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMapView, 3833646656, 41784, 19162, 173, 207, 39, 34, 114, 228, 140, 185);
		RT_INTERFACE!{interface IMapView<K, V>(IMapViewVtbl): IInspectable(IInspectableVtbl) [IID_IMapView] {
			fn Lookup(&mut self, key: K::Abi, out: *mut V::Abi) -> ::w::HRESULT,
			fn get_Size(&mut self, out: *mut u32) -> ::w::HRESULT,
			fn HasKey(&mut self, key: K::Abi, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn Split(&mut self, first: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<K, V>, second: *mut *mut ::rt::gen::windows::foundation::collections::IMapView<K, V>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_VectorChangedEventHandler, 201660242, 40895, 19568, 170, 12, 14, 76, 130, 217, 167, 97);
		RT_DELEGATE!{delegate VectorChangedEventHandler<T>(VectorChangedEventHandlerVtbl, VectorChangedEventHandlerImpl) [IID_VectorChangedEventHandler] {
			fn Invoke(&mut self, sender: *mut ::rt::gen::windows::foundation::collections::IObservableVector<T>, event: *mut ::rt::gen::windows::foundation::collections::IVectorChangedEventArgs) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IObservableVector, 1494739795, 20660, 18957, 179, 9, 101, 134, 43, 63, 29, 188);
		RT_INTERFACE!{interface IObservableVector<T>(IObservableVectorVtbl): IInspectable(IInspectableVtbl) [IID_IObservableVector] {
			fn add_VectorChanged(&mut self, vhnd: *mut ::rt::gen::windows::foundation::collections::VectorChangedEventHandler<T>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_VectorChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IMapChangedEventArgs, 2570712287, 1290, 19471, 170, 96, 119, 7, 95, 156, 71, 119);
		RT_INTERFACE!{interface IMapChangedEventArgs<K>(IMapChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_IMapChangedEventArgs] {
			fn get_CollectionChange(&mut self, out: *mut ::rt::gen::windows::foundation::collections::CollectionChange) -> ::w::HRESULT,
			fn get_Key(&mut self, out: *mut K::Abi) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_MapChangedEventHandler, 395646963, 38126, 16888, 189, 220, 118, 138, 137, 85, 68, 243);
		RT_DELEGATE!{delegate MapChangedEventHandler<K, V>(MapChangedEventHandlerVtbl, MapChangedEventHandlerImpl) [IID_MapChangedEventHandler] {
			fn Invoke(&mut self, sender: *mut ::rt::gen::windows::foundation::collections::IObservableMap<K, V>, event: *mut ::rt::gen::windows::foundation::collections::IMapChangedEventArgs<K>) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IObservableMap, 1709124597, 48953, 16821, 174, 188, 90, 157, 134, 94, 71, 43);
		RT_INTERFACE!{interface IObservableMap<K, V>(IObservableMapVtbl): IInspectable(IInspectableVtbl) [IID_IObservableMap] {
			fn add_MapChanged(&mut self, vhnd: *mut ::rt::gen::windows::foundation::collections::MapChangedEventHandler<K, V>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_MapChanged(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::adc::provider::IAdcControllerProvider> => [0x7c4789c0,0x8445,0x5757,0xaa,0xb7,0x65,0x9c,0xbf,0x50,0xaa,0xa7] as IID_IVectorView_1_Windows_Devices_Adc_Provider_IAdcControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::adc::provider::IAdcControllerProvider> => [0x30047155,0x1f71,0x5223,0x84,0x82,0xe5,0x15,0x9d,0x01,0x37,0xd0] as IID_IIterable_1_Windows_Devices_Adc_Provider_IAdcControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::adc::provider::IAdcControllerProvider> => [0xb43acf15,0xa24a,0x5b00,0xb7,0x10,0x17,0x37,0xba,0x55,0x0a,0x18] as IID_IIterator_1_Windows_Devices_Adc_Provider_IAdcControllerProvider }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::adc::AdcController> => [0x27547dc1,0x376e,0x5ce4,0xa2,0x46,0x34,0xf2,0x10,0xc8,0x44,0x3c] as IID_IVectorView_1_Windows_Devices_Adc_AdcController }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::adc::AdcController> => [0x4e478aad,0x4861,0x5758,0xb6,0x4b,0x5b,0x4f,0x28,0xd8,0xf8,0x6e] as IID_IIterable_1_Windows_Devices_Adc_AdcController }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::adc::AdcController> => [0xa10b62c1,0xa014,0x5335,0x88,0x67,0x74,0x7f,0xca,0xb1,0x60,0x05] as IID_IIterator_1_Windows_Devices_Adc_AdcController }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::gpio::provider::IGpioControllerProvider> => [0xf429355f,0x7a16,0x5dcf,0xa5,0x75,0xdb,0x7d,0x2a,0x20,0xec,0xed] as IID_IVectorView_1_Windows_Devices_Gpio_Provider_IGpioControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::gpio::provider::IGpioControllerProvider> => [0x09212bd4,0x851b,0x52bd,0xb8,0x2c,0x42,0x1b,0xf3,0xd6,0xf5,0x11] as IID_IIterable_1_Windows_Devices_Gpio_Provider_IGpioControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::gpio::provider::IGpioControllerProvider> => [0x6ac0edb9,0xe3c9,0x5840,0x8a,0xa8,0x1b,0xc4,0x53,0x66,0xf6,0xca] as IID_IIterator_1_Windows_Devices_Gpio_Provider_IGpioControllerProvider }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::gpio::GpioController> => [0x7fc72a82,0x2c57,0x5c01,0xa6,0x52,0xa8,0xbd,0xac,0x68,0x5d,0x30] as IID_IVectorView_1_Windows_Devices_Gpio_GpioController }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::gpio::GpioController> => [0x415c3794,0xb2b6,0x5f5c,0x9a,0x05,0xae,0x92,0x68,0x51,0x47,0x26] as IID_IIterable_1_Windows_Devices_Gpio_GpioController }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::gpio::GpioController> => [0x67944db0,0x6c56,0x5a2f,0x9e,0x7b,0x63,0xca,0x1a,0xa8,0xc4,0x11] as IID_IIterator_1_Windows_Devices_Gpio_GpioController }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::i2c::provider::II2cControllerProvider> => [0x511f8a39,0x98ca,0x550d,0xaf,0x25,0x1d,0xf2,0xc1,0x19,0x3c,0x01] as IID_IVectorView_1_Windows_Devices_I2c_Provider_II2cControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::i2c::provider::II2cControllerProvider> => [0x11341a6c,0x3a02,0x5f73,0x9d,0xb8,0xc3,0xec,0x58,0x23,0xe3,0x5d] as IID_IIterable_1_Windows_Devices_I2c_Provider_II2cControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::i2c::provider::II2cControllerProvider> => [0xf6232961,0xc660,0x50a1,0x82,0xe8,0x12,0x89,0x2f,0xcd,0x91,0xf7] as IID_IIterator_1_Windows_Devices_I2c_Provider_II2cControllerProvider }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::i2c::I2cController> => [0xc5dd481d,0xa441,0x5a8c,0x86,0x5f,0x08,0xfa,0x31,0x49,0x0d,0xe5] as IID_IVectorView_1_Windows_Devices_I2c_I2cController }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::i2c::I2cController> => [0xa5ee8233,0x2429,0x5b26,0x9a,0x02,0x99,0x3e,0x4e,0x7e,0xdf,0xa9] as IID_IIterable_1_Windows_Devices_I2c_I2cController }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::i2c::I2cController> => [0x8f6822fc,0xe4ea,0x5b35,0x93,0x9a,0x27,0xf3,0xb3,0xd5,0x50,0xd2] as IID_IIterator_1_Windows_Devices_I2c_I2cController }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::pwm::provider::IPwmControllerProvider> => [0x1a166093,0x1a7a,0x5e12,0x9d,0x38,0xf8,0x92,0xfe,0xc3,0xec,0x66] as IID_IVectorView_1_Windows_Devices_Pwm_Provider_IPwmControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::pwm::provider::IPwmControllerProvider> => [0x4936ed59,0xb494,0x5128,0xbc,0x7e,0x03,0xe6,0x30,0x34,0x64,0x75] as IID_IIterable_1_Windows_Devices_Pwm_Provider_IPwmControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::pwm::provider::IPwmControllerProvider> => [0x90389702,0xf036,0x56e1,0xa9,0x4f,0x6d,0x99,0xd5,0x2b,0x95,0x78] as IID_IIterator_1_Windows_Devices_Pwm_Provider_IPwmControllerProvider }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::pwm::PwmController> => [0x07cb8bac,0x3bac,0x5ea0,0x91,0x9a,0x9e,0xaf,0x62,0x02,0x70,0xae] as IID_IVectorView_1_Windows_Devices_Pwm_PwmController }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::pwm::PwmController> => [0x1403a6ab,0x73cb,0x5805,0x9b,0xbc,0xa0,0xdd,0x39,0xd4,0x76,0xb0] as IID_IIterable_1_Windows_Devices_Pwm_PwmController }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::pwm::PwmController> => [0x599330bd,0xb0ca,0x533e,0x93,0x8f,0x5d,0xd4,0x24,0x2b,0xf5,0x13] as IID_IIterator_1_Windows_Devices_Pwm_PwmController }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::spi::provider::ISpiControllerProvider> => [0x96a4919b,0x3229,0x5e41,0x8b,0x10,0xc8,0x19,0x8c,0x44,0xf1,0x0c] as IID_IVectorView_1_Windows_Devices_Spi_Provider_ISpiControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::spi::provider::ISpiControllerProvider> => [0x71ba027d,0x8c84,0x58b1,0x8d,0x66,0x91,0x77,0xc1,0x16,0x98,0xeb] as IID_IIterable_1_Windows_Devices_Spi_Provider_ISpiControllerProvider }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::spi::provider::ISpiControllerProvider> => [0xcf1d15d3,0xa6c8,0x56dd,0x80,0xc8,0xe8,0xd9,0x60,0x26,0x22,0x77] as IID_IIterator_1_Windows_Devices_Spi_Provider_ISpiControllerProvider }
		RT_PINTERFACE!{ for IVectorView<i32> => [0x8d720cdf,0x3934,0x5d3f,0x9a,0x55,0x40,0xe8,0x06,0x3b,0x08,0x6a] as IID_IVectorView_1_System_Int32 }
		RT_PINTERFACE!{ for IIterable<i32> => [0x81a643fb,0xf51c,0x5565,0x83,0xc4,0xf9,0x64,0x25,0x77,0x7b,0x66] as IID_IIterable_1_System_Int32 }
		RT_PINTERFACE!{ for IIterator<i32> => [0xbfea7f78,0x50c2,0x5f1d,0xa6,0xea,0x9e,0x97,0x8d,0x26,0x99,0xff] as IID_IIterator_1_System_Int32 }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::spi::SpiController> => [0x35fec489,0x44a2,0x5543,0x8a,0x0c,0xb5,0x2e,0x2f,0x9c,0xf8,0x7c] as IID_IVectorView_1_Windows_Devices_Spi_SpiController }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::spi::SpiController> => [0x7b076938,0xdc1b,0x5368,0x90,0x03,0x05,0x92,0x91,0xd3,0x7f,0x35] as IID_IIterable_1_Windows_Devices_Spi_SpiController }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::spi::SpiController> => [0xfd7d5997,0x544c,0x5be9,0xb0,0xfa,0x1d,0x0e,0xfb,0xfc,0x4a,0x03] as IID_IIterator_1_Windows_Devices_Spi_SpiController }
		RT_PINTERFACE!{ for<'a> IMap<&'a str, &'a str> => [0xf6d1f700,0x49c2,0x52ae,0x81,0x54,0x82,0x6f,0x99,0x08,0x77,0x3c] as IID_IMap_2_System_String_System_String }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<&'a str, &'a str>> => [0xe9bdaaf0,0xcbf6,0x5c72,0xbe,0x90,0x29,0xcb,0xf3,0xa1,0x31,0x9b] as IID_IIterable_1_Windows_Foundation_Collections_IKeyValuePair_2_System_String_System_String }
		RT_PINTERFACE!{ for<'a> IKeyValuePair<&'a str, &'a str> => [0x60310303,0x49c5,0x52e6,0xab,0xc6,0xa9,0xb3,0x6e,0xcc,0xc7,0x16] as IID_IKeyValuePair_2_System_String_System_String }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<&'a str, &'a str>> => [0x05eb86f1,0x7140,0x5517,0xb8,0x8d,0xcb,0xae,0xbe,0x57,0xe6,0xb1] as IID_IIterator_1_Windows_Foundation_Collections_IKeyValuePair_2_System_String_System_String }
		RT_PINTERFACE!{ for<'a> IMapView<&'a str, &'a str> => [0xac7f26f2,0xfeb7,0x5b2a,0x8a,0xc4,0x34,0x5b,0xc6,0x2c,0xae,0xde] as IID_IMapView_2_System_String_System_String }
		RT_PINTERFACE!{ for<'a> IVector<&'a str> => [0x98b9acc1,0x4b56,0x532e,0xac,0x73,0x03,0xd5,0x29,0x1c,0xca,0x90] as IID_IVector_1_System_String }
		RT_PINTERFACE!{ for<'a> IIterable<&'a str> => [0xe2fcc7c1,0x3bfc,0x5a0b,0xb2,0xb0,0x72,0xe7,0x69,0xd1,0xcb,0x7e] as IID_IIterable_1_System_String }
		RT_PINTERFACE!{ for<'a> IIterator<&'a str> => [0x8c304ebb,0x6615,0x50a4,0x88,0x29,0x87,0x9e,0xcd,0x44,0x32,0x36] as IID_IIterator_1_System_String }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a str> => [0x2f13c006,0xa03a,0x5f69,0xb0,0x90,0x75,0xa4,0x3e,0x33,0x42,0x3e] as IID_IVectorView_1_System_String }
		RT_PINTERFACE!{ for IVector<i32> => [0xb939af5b,0xb45d,0x5489,0x91,0x49,0x61,0x44,0x2c,0x19,0x05,0xfe] as IID_IVector_1_System_Int32 }
		RT_PINTERFACE!{ for IVector<::rt::gen::windows::devices::sms::SmsBroadcastType> => [0xd76bef0b,0x1358,0x5895,0xbd,0x42,0xf1,0x7f,0x6f,0x33,0xee,0xd1] as IID_IVector_1_Windows_Devices_Sms_SmsBroadcastType }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::sms::SmsBroadcastType> => [0x12276b75,0x173e,0x514b,0x98,0xf0,0x8a,0x79,0x27,0xa9,0x20,0x6c] as IID_IIterable_1_Windows_Devices_Sms_SmsBroadcastType }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::sms::SmsBroadcastType> => [0x6448ddea,0xc1cd,0x5143,0xa4,0x22,0x5f,0xe4,0xf0,0x08,0xcc,0x92] as IID_IIterator_1_Windows_Devices_Sms_SmsBroadcastType }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::sms::SmsBroadcastType> => [0x4063e791,0xda2d,0x5e4c,0x91,0x13,0x5b,0x6b,0xa0,0xa7,0xc5,0x95] as IID_IVectorView_1_Windows_Devices_Sms_SmsBroadcastType }
		RT_PINTERFACE!{ for<'a> IVector<&'a ::rt::gen::windows::devices::sms::SmsFilterRule> => [0x8e72fa52,0x4867,0x5696,0xb4,0xd9,0x4c,0xa2,0x3f,0x19,0xe1,0x77] as IID_IVector_1_Windows_Devices_Sms_SmsFilterRule }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::sms::SmsFilterRule> => [0x03ed8267,0x9c90,0x5260,0x8b,0xc0,0x6c,0x3e,0x33,0x06,0xc9,0x64] as IID_IIterable_1_Windows_Devices_Sms_SmsFilterRule }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::sms::SmsFilterRule> => [0xfec7cae6,0x97f5,0x5a19,0xb3,0x2a,0xd8,0xf0,0xba,0x27,0x6f,0x34] as IID_IIterator_1_Windows_Devices_Sms_SmsFilterRule }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::sms::SmsFilterRule> => [0xf3ed4299,0xde36,0x5d82,0x99,0x3f,0x35,0xfc,0x67,0x7d,0x8b,0x72] as IID_IVectorView_1_Windows_Devices_Sms_SmsFilterRule }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::sms::SmsMessageRegistration> => [0x23aaa815,0x24ef,0x57c4,0xb1,0xbc,0x94,0xd6,0x2c,0x0a,0x59,0xa3] as IID_IVectorView_1_Windows_Devices_Sms_SmsMessageRegistration }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::sms::SmsMessageRegistration> => [0xf836fa0a,0x770d,0x5e8f,0x86,0x64,0x01,0xc4,0x3f,0x95,0x9e,0xea] as IID_IIterable_1_Windows_Devices_Sms_SmsMessageRegistration }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::sms::SmsMessageRegistration> => [0xee1a0675,0xf3c9,0x5c12,0x93,0xe0,0xf2,0xb0,0x1b,0xdc,0xe6,0x11] as IID_IIterator_1_Windows_Devices_Sms_SmsMessageRegistration }
		RT_PINTERFACE!{ for IVector<::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationMechanism> => [0x19c16b93,0xf9ca,0x5c05,0xbf,0x73,0xe7,0x4c,0xd0,0x54,0xc5,0x87] as IID_IVector_1_Windows_Devices_AllJoyn_AllJoynAuthenticationMechanism }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationMechanism> => [0xd307c7af,0x4106,0x5d1c,0xb0,0x6c,0x5e,0xb5,0x93,0xd9,0xbe,0x34] as IID_IIterable_1_Windows_Devices_AllJoyn_AllJoynAuthenticationMechanism }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationMechanism> => [0x0fbc36d2,0xf46e,0x5a4d,0xaa,0x10,0x4c,0x80,0x6b,0x49,0x45,0xd6] as IID_IIterator_1_Windows_Devices_AllJoyn_AllJoynAuthenticationMechanism }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::alljoyn::AllJoynAuthenticationMechanism> => [0xeaf57f86,0xb059,0x5ac2,0x9c,0x1f,0xd3,0xc6,0x5e,0xac,0x67,0xa3] as IID_IVectorView_1_Windows_Devices_AllJoyn_AllJoynAuthenticationMechanism }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::security::cryptography::certificates::Certificate> => [0x0c7d1423,0xe8fd,0x5a91,0xb5,0x5c,0x8b,0xfb,0xe7,0xac,0x2d,0x40] as IID_IIterable_1_Windows_Security_Cryptography_Certificates_Certificate }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::security::cryptography::certificates::Certificate> => [0x676fc159,0xf15c,0x58bd,0x91,0xa7,0x28,0xf7,0xe7,0x95,0xc7,0x56] as IID_IIterator_1_Windows_Security_Cryptography_Certificates_Certificate }
		RT_PINTERFACE!{ for<'a> IVector<&'a ::rt::gen::windows::security::cryptography::certificates::Certificate> => [0x36282c0f,0x2f1f,0x57f4,0xb2,0xb1,0x86,0x7a,0xf9,0x0c,0x3d,0x13] as IID_IVector_1_Windows_Security_Cryptography_Certificates_Certificate }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::security::cryptography::certificates::Certificate> => [0x963f7013,0x77c2,0x51c5,0x80,0x38,0xb5,0xbc,0xef,0x63,0x3e,0xdb] as IID_IVectorView_1_Windows_Security_Cryptography_Certificates_Certificate }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::security::cryptography::certificates::ChainValidationResult> => [0xcb383486,0xc2bc,0x5756,0x91,0x2d,0x6a,0x70,0x8a,0x07,0xe5,0xbd] as IID_IVectorView_1_Windows_Security_Cryptography_Certificates_ChainValidationResult }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::security::cryptography::certificates::ChainValidationResult> => [0x2628f58f,0x3f02,0x54f2,0x80,0x8f,0xe1,0x11,0x77,0x09,0xd6,0xd0] as IID_IIterable_1_Windows_Security_Cryptography_Certificates_ChainValidationResult }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::security::cryptography::certificates::ChainValidationResult> => [0x8bcad2b7,0x0e3b,0x5eae,0xbf,0x69,0xe1,0xf6,0xd9,0xc8,0x88,0xf8] as IID_IIterator_1_Windows_Security_Cryptography_Certificates_ChainValidationResult }
		RT_PINTERFACE!{ for<'a> IMapView<&'a str, &'a IInspectable> => [0xbb78502a,0xf79d,0x54fa,0x92,0xc9,0x90,0xc5,0x03,0x9f,0xdf,0x7e] as IID_IMapView_2_System_String_System_Object }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<&'a str, &'a IInspectable>> => [0xfe2f3d47,0x5d47,0x5499,0x83,0x74,0x43,0x0c,0x7c,0xda,0x02,0x04] as IID_IIterable_1_Windows_Foundation_Collections_IKeyValuePair_2_System_String_System_Object }
		RT_PINTERFACE!{ for<'a> IKeyValuePair<&'a str, &'a IInspectable> => [0x09335560,0x6c6b,0x5a26,0x93,0x48,0x97,0xb7,0x81,0x13,0x2b,0x20] as IID_IKeyValuePair_2_System_String_System_Object }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<&'a str, &'a IInspectable>> => [0x5db5fa32,0x707c,0x5849,0xa0,0x6b,0x91,0xc8,0xeb,0x9d,0x10,0xe8] as IID_IIterator_1_Windows_Foundation_Collections_IKeyValuePair_2_System_String_System_Object }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::globalization::Language> => [0x144b0f3d,0x2d59,0x5dd2,0xb0,0x12,0x90,0x8e,0xc3,0xe0,0x64,0x35] as IID_IVectorView_1_Windows_Globalization_Language }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::globalization::Language> => [0x48409a10,0x61b6,0x5db1,0xa6,0x9d,0x8a,0xbc,0x46,0xac,0x60,0x8a] as IID_IIterable_1_Windows_Globalization_Language }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::globalization::Language> => [0x30e99ae6,0xf414,0x5243,0x8d,0xb2,0xaa,0xb3,0x8e,0xa3,0xf1,0xf1] as IID_IIterator_1_Windows_Globalization_Language }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::storage::streams::IBuffer> => [0xfd944562,0x11d6,0x5eab,0xbd,0x72,0x70,0x19,0x93,0xb6,0x8f,0xac] as IID_IVectorView_1_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::storage::streams::IBuffer> => [0x902972bf,0xa984,0x5443,0xb1,0xc5,0x2f,0x04,0xa9,0x9e,0x1f,0xca] as IID_IIterable_1_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::storage::streams::IBuffer> => [0xafee38e0,0xf882,0x5f10,0x96,0x55,0x1f,0xc9,0x8c,0xc8,0xcc,0xe5] as IID_IIterator_1_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommDeviceService> => [0x97df6b82,0xd15c,0x597e,0xba,0x69,0x49,0x22,0x07,0xa1,0xc1,0x08] as IID_IVectorView_1_Windows_Devices_Bluetooth_Rfcomm_RfcommDeviceService }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommDeviceService> => [0x3378e9a6,0xf6e2,0x50ea,0xbf,0xee,0xb8,0x10,0x96,0x31,0xfe,0xca] as IID_IIterable_1_Windows_Devices_Bluetooth_Rfcomm_RfcommDeviceService }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::rfcomm::RfcommDeviceService> => [0x64ab0132,0xc64c,0x5a87,0x81,0x13,0x61,0x3e,0xf3,0x56,0x92,0x4c] as IID_IIterator_1_Windows_Devices_Bluetooth_Rfcomm_RfcommDeviceService }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService> => [0x7c8e7fdd,0xa1a1,0x528a,0x81,0xd1,0x29,0x67,0x69,0x22,0x7a,0x08] as IID_IVectorView_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattDeviceService }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService> => [0x4b192e23,0x4893,0x56b2,0x8e,0xff,0x43,0x9c,0x3a,0xb7,0xfd,0x1f] as IID_IIterable_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattDeviceService }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDeviceService> => [0x8beb3a26,0x73ca,0x50f3,0xa1,0xd3,0x41,0x8c,0x60,0xa9,0xf3,0xb2] as IID_IIterator_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattDeviceService }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDescriptor> => [0x19605ea8,0x73d6,0x5760,0x84,0x9b,0xfe,0x5f,0x8a,0x2b,0xd0,0x5c] as IID_IVectorView_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattDescriptor }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDescriptor> => [0x30e2ffc4,0x3aa3,0x5219,0x9a,0x18,0xca,0x2d,0x0b,0x65,0x62,0xe8] as IID_IIterable_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattDescriptor }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattDescriptor> => [0x3d8df436,0xcefb,0x5ffb,0x85,0x8c,0x48,0x82,0xce,0x1d,0xa0,0x79] as IID_IIterator_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattDescriptor }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattPresentationFormat> => [0x0ea2c154,0x22b8,0x5c8e,0x92,0x5d,0xd4,0x7e,0x1a,0xad,0x31,0xbb] as IID_IVectorView_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattPresentationFormat }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattPresentationFormat> => [0xd75fcef1,0xc10e,0x5b7b,0xb1,0x30,0xf5,0xa0,0x03,0x14,0xd3,0x5d] as IID_IIterable_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattPresentationFormat }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattPresentationFormat> => [0x20006c53,0x5dda,0x5319,0x91,0xb1,0xc6,0xf2,0x8f,0xe6,0x59,0x33] as IID_IIterator_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattPresentationFormat }
		RT_PINTERFACE!{ for<'a> IMapView<u32, &'a ::rt::gen::windows::storage::streams::IBuffer> => [0x57dc41e6,0x8b4d,0x5910,0x97,0x03,0xd7,0xc6,0x68,0x43,0x68,0x52] as IID_IMapView_2_System_UInt32_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<u32, &'a ::rt::gen::windows::storage::streams::IBuffer>> => [0x4fe7fe23,0x22b1,0x528c,0x88,0x1d,0xa4,0xec,0xea,0xef,0x0f,0x11] as IID_IIterable_1_Windows_Foundation_Collections_IKeyValuePair_2_System_UInt32_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> IKeyValuePair<u32, &'a ::rt::gen::windows::storage::streams::IBuffer> => [0x82a3a3b7,0xe04a,0x5395,0x84,0x87,0x7f,0x94,0xf1,0x50,0x8c,0xe7] as IID_IKeyValuePair_2_System_UInt32_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<u32, &'a ::rt::gen::windows::storage::streams::IBuffer>> => [0xa295fa0c,0xc99f,0x5109,0x8a,0xb9,0x91,0x53,0x4b,0xb4,0x8c,0x9b] as IID_IIterator_1_Windows_Foundation_Collections_IKeyValuePair_2_System_UInt32_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for<'a> IMap<u32, &'a ::rt::gen::windows::storage::streams::IBuffer> => [0x5d2591df,0x48c5,0x5734,0x9e,0xf1,0xbd,0x63,0x9b,0x03,0x20,0x07] as IID_IMap_2_System_UInt32_Windows_Storage_Streams_IBuffer }
		RT_PINTERFACE!{ for IVector<::Guid> => [0x482e676d,0xb913,0x5ec1,0xaf,0xa8,0x5f,0x96,0x92,0x2e,0x94,0xae] as IID_IVector_1_System_Guid }
		RT_PINTERFACE!{ for IIterable<::Guid> => [0xf4ca3045,0x5dd7,0x54be,0x98,0x2e,0xd8,0x8d,0x8c,0xa0,0x87,0x6e] as IID_IIterable_1_System_Guid }
		RT_PINTERFACE!{ for IIterator<::Guid> => [0xd3d64048,0x82b3,0x53c7,0x92,0x85,0xb0,0xbe,0x18,0x36,0x84,0x82] as IID_IIterator_1_System_Guid }
		RT_PINTERFACE!{ for IVectorView<::Guid> => [0x9520e64b,0x15b2,0x52a6,0x98,0xed,0x31,0x91,0xfa,0x6c,0xf6,0x8a] as IID_IVectorView_1_System_Guid }
		RT_PINTERFACE!{ for<'a> IVector<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEManufacturerData> => [0x52d75b45,0x1d24,0x5eeb,0xba,0xbb,0x65,0xef,0xfa,0xe4,0x5e,0x46] as IID_IVector_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEManufacturerData }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEManufacturerData> => [0x834a4cac,0xbb8b,0x5f0f,0x9f,0x28,0x4d,0xbc,0x98,0xc1,0x79,0x07] as IID_IIterable_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEManufacturerData }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEManufacturerData> => [0x12f158dd,0x7016,0x5338,0xac,0x5c,0x7d,0x55,0x03,0xd7,0x32,0x74] as IID_IIterator_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEManufacturerData }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEManufacturerData> => [0x78ab070e,0xad7e,0x5912,0xa4,0xf1,0x7b,0xe3,0x3e,0x45,0x60,0xaf] as IID_IVectorView_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEManufacturerData }
		RT_PINTERFACE!{ for<'a> IVector<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementDataSection> => [0xb6f71ad2,0xe2cf,0x5d54,0xb6,0xf1,0x90,0x96,0x4e,0xe5,0xd4,0xda] as IID_IVector_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementDataSection }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementDataSection> => [0xdb98b5d1,0x897e,0x59cc,0xb8,0x6a,0x7b,0x88,0x55,0xac,0x98,0xaf] as IID_IIterable_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementDataSection }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementDataSection> => [0xaf5c0e81,0x788b,0x52d4,0x82,0xa2,0x1e,0xd2,0x8c,0x66,0xa0,0x5e] as IID_IIterator_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementDataSection }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementDataSection> => [0xc4f2b8ea,0x11a8,0x5109,0x90,0x13,0x40,0x47,0xe1,0x2c,0x72,0xe8] as IID_IVectorView_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementDataSection }
		RT_PINTERFACE!{ for<'a> IVector<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementBytePattern> => [0x8dd461b7,0x9775,0x5e82,0xa0,0xa6,0x66,0x27,0xab,0xd0,0xd0,0x10] as IID_IVector_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementBytePattern }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementBytePattern> => [0x1e3fadee,0x54ac,0x538b,0x87,0x77,0x35,0x1a,0xfb,0x78,0xcb,0x74] as IID_IIterable_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementBytePattern }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementBytePattern> => [0xb33e103a,0x1a61,0x5107,0x88,0x13,0xc0,0xe9,0x05,0xc0,0x54,0x86] as IID_IIterator_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementBytePattern }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementBytePattern> => [0xa7d9983a,0xa11f,0x572e,0x89,0xfb,0x68,0x3e,0xa4,0x29,0xbc,0xbc] as IID_IVectorView_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementBytePattern }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementReceivedEventArgs> => [0x8aef9bca,0xfe7d,0x5966,0x97,0x89,0xfe,0xde,0x24,0xcb,0x41,0xc4] as IID_IVectorView_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementReceivedEventArgs> => [0x34f6412f,0x8314,0x5205,0x96,0x7c,0xdb,0x35,0x7c,0x9a,0x42,0xa7] as IID_IIterable_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementReceivedEventArgs }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::advertisement::BluetoothLEAdvertisementReceivedEventArgs> => [0x096edbb8,0xecef,0x5724,0xbe,0x62,0x24,0x0d,0xcf,0xf6,0xac,0xa9] as IID_IIterator_1_Windows_Devices_Bluetooth_Advertisement_BluetoothLEAdvertisementReceivedEventArgs }
		RT_PINTERFACE!{ for IVector<::rt::gen::windows::devices::enumeration::DeviceClass> => [0xee662d37,0xb0eb,0x5729,0x98,0x32,0x15,0x6f,0xd2,0x88,0x9d,0x48] as IID_IVector_1_Windows_Devices_Enumeration_DeviceClass }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::enumeration::DeviceClass> => [0x47d4be05,0x58f1,0x522e,0x81,0xc6,0x97,0x5e,0xb4,0x13,0x1b,0xb9] as IID_IIterable_1_Windows_Devices_Enumeration_DeviceClass }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::enumeration::DeviceClass> => [0xc3807283,0x1416,0x593c,0x95,0x5c,0x0b,0x4a,0x28,0x6f,0xf7,0xbb] as IID_IIterator_1_Windows_Devices_Enumeration_DeviceClass }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::enumeration::DeviceClass> => [0x01a80a97,0xbd87,0x5c8a,0x97,0xfd,0xd4,0x49,0xc9,0x8b,0xda,0xc6] as IID_IVectorView_1_Windows_Devices_Enumeration_DeviceClass }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::enumeration::DeviceInformation> => [0xe170688f,0x3495,0x5bf6,0xaa,0xb5,0x9c,0xac,0x17,0xe0,0xf1,0x0f] as IID_IVectorView_1_Windows_Devices_Enumeration_DeviceInformation }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::enumeration::DeviceInformation> => [0xdd9f8a5d,0xec98,0x5f4b,0xa3,0xea,0x9c,0x8b,0x5a,0xd5,0x3c,0x4b] as IID_IIterable_1_Windows_Devices_Enumeration_DeviceInformation }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::enumeration::DeviceInformation> => [0x6f85d843,0xe8ab,0x5b46,0x85,0xd7,0x32,0x7c,0x58,0xd1,0x87,0x12] as IID_IIterator_1_Windows_Devices_Enumeration_DeviceInformation }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::enumeration::DeviceWatcherEventKind> => [0xf04365ab,0xd3f3,0x5f85,0xa7,0xda,0xdc,0x19,0xcf,0xf7,0x3d,0x86] as IID_IIterable_1_Windows_Devices_Enumeration_DeviceWatcherEventKind }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::enumeration::DeviceWatcherEventKind> => [0xcb5ca9db,0xccd6,0x5103,0xa9,0x3d,0xc9,0x25,0xc9,0x08,0x83,0x8d] as IID_IIterator_1_Windows_Devices_Enumeration_DeviceWatcherEventKind }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::enumeration::DeviceWatcherEvent> => [0x8f994d37,0x8fab,0x51c6,0xa1,0xe0,0xc9,0x3f,0x68,0xa2,0x0e,0xf0] as IID_IVectorView_1_Windows_Devices_Enumeration_DeviceWatcherEvent }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::enumeration::DeviceWatcherEvent> => [0xb48fd051,0xeafa,0x523f,0xa6,0x6e,0x9d,0x41,0x51,0xc5,0xd5,0x22] as IID_IIterable_1_Windows_Devices_Enumeration_DeviceWatcherEvent }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::enumeration::DeviceWatcherEvent> => [0x74f7d6cc,0x9c20,0x5bb9,0xba,0xce,0xb2,0xff,0xa3,0x86,0x87,0xf9] as IID_IIterator_1_Windows_Devices_Enumeration_DeviceWatcherEvent }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObject> => [0xcce5a798,0xd269,0x5fce,0x99,0xce,0xef,0x0a,0xe3,0xcd,0x05,0x69] as IID_IVectorView_1_Windows_Devices_Enumeration_Pnp_PnpObject }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObject> => [0x30b50092,0x36ee,0x53ff,0x94,0x50,0x02,0x90,0x04,0x43,0x6c,0x60] as IID_IIterable_1_Windows_Devices_Enumeration_Pnp_PnpObject }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::enumeration::pnp::PnpObject> => [0x6bb6d2f1,0xb5fb,0x57f0,0x82,0x51,0xf2,0x0c,0xde,0x5a,0x68,0x71] as IID_IIterator_1_Windows_Devices_Enumeration_Pnp_PnpObject }
		RT_PINTERFACE!{ for<'a> IVector<&'a ::rt::gen::windows::devices::geolocation::geofencing::Geofence> => [0x096dc936,0x5f66,0x5c6e,0x95,0xce,0xef,0x55,0x41,0xfb,0xf4,0xc4] as IID_IVector_1_Windows_Devices_Geolocation_Geofencing_Geofence }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::geolocation::geofencing::Geofence> => [0xce697733,0x595c,0x51c0,0xad,0x5f,0x32,0x4a,0xf5,0xcd,0xf2,0xdd] as IID_IIterable_1_Windows_Devices_Geolocation_Geofencing_Geofence }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::geolocation::geofencing::Geofence> => [0xe7a9e144,0x202d,0x5708,0xa9,0xbd,0xe3,0xdc,0x0e,0x14,0xdf,0x46] as IID_IIterator_1_Windows_Devices_Geolocation_Geofencing_Geofence }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::geolocation::geofencing::Geofence> => [0xd8039aca,0x1a45,0x5d13,0x8c,0xfd,0x49,0x00,0xc2,0x2b,0x8e,0xf1] as IID_IVectorView_1_Windows_Devices_Geolocation_Geofencing_Geofence }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::geolocation::geofencing::GeofenceStateChangeReport> => [0xea91593d,0xecf4,0x5041,0x86,0xf2,0x83,0x7a,0x28,0x2c,0x4d,0x94] as IID_IVectorView_1_Windows_Devices_Geolocation_Geofencing_GeofenceStateChangeReport }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::geolocation::geofencing::GeofenceStateChangeReport> => [0x76f50b4e,0x7aa7,0x565b,0xaa,0xda,0xb0,0xc1,0xcc,0x14,0x4e,0xd0] as IID_IIterable_1_Windows_Devices_Geolocation_Geofencing_GeofenceStateChangeReport }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::geolocation::geofencing::GeofenceStateChangeReport> => [0xeaff2de4,0x6650,0x544a,0xb7,0xac,0x6d,0x5b,0x81,0x9d,0x46,0x98] as IID_IIterator_1_Windows_Devices_Geolocation_Geofencing_GeofenceStateChangeReport }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::geolocation::BasicGeoposition> => [0x177f5719,0xe234,0x59db,0x99,0xba,0xf7,0xfd,0xdd,0xf3,0x14,0x30] as IID_IVectorView_1_Windows_Devices_Geolocation_BasicGeoposition }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::geolocation::BasicGeoposition> => [0x922399a8,0x0093,0x5009,0xa8,0xd2,0xf8,0x7b,0x0e,0xae,0x75,0xf5] as IID_IIterable_1_Windows_Devices_Geolocation_BasicGeoposition }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::geolocation::BasicGeoposition> => [0x1b4e26a1,0x88e4,0x5872,0xbb,0x2d,0x4f,0x31,0x70,0x08,0x28,0xb2] as IID_IIterator_1_Windows_Devices_Geolocation_BasicGeoposition }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::geolocation::Geoposition> => [0xd572ccf3,0x0c60,0x553f,0xa6,0x24,0xc7,0x16,0x48,0xaf,0x8e,0x7a] as IID_IVectorView_1_Windows_Devices_Geolocation_Geoposition }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::geolocation::Geoposition> => [0x135ed72d,0x75b1,0x5881,0xbe,0x41,0x6f,0xfe,0xaa,0x20,0x20,0x44] as IID_IIterable_1_Windows_Devices_Geolocation_Geoposition }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::geolocation::Geoposition> => [0xa99b4206,0x263e,0x5308,0x82,0xf2,0x31,0x31,0x5c,0x65,0x13,0x5c] as IID_IIterator_1_Windows_Devices_Geolocation_Geoposition }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidCollection> => [0x96f1faac,0x348f,0x5b8f,0xa7,0x1d,0x2d,0x65,0x0e,0x0b,0x11,0xf2] as IID_IVectorView_1_Windows_Devices_HumanInterfaceDevice_HidCollection }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidCollection> => [0xbbeada0f,0x708f,0x5b5e,0xa0,0x17,0x5c,0x64,0xff,0xb9,0x6b,0x69] as IID_IIterable_1_Windows_Devices_HumanInterfaceDevice_HidCollection }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidCollection> => [0xcefcee70,0xc7ff,0x57c1,0xa6,0x75,0xa0,0xdf,0x89,0x76,0xa9,0x88] as IID_IIterator_1_Windows_Devices_HumanInterfaceDevice_HidCollection }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl> => [0x0e417dac,0x591a,0x5de0,0xaf,0xd6,0x0b,0x2c,0x04,0xc3,0x04,0xe7] as IID_IVectorView_1_Windows_Devices_HumanInterfaceDevice_HidBooleanControl }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl> => [0x1111e585,0x5ab0,0x5d2b,0x8a,0xed,0xb6,0xd6,0x18,0x6d,0x1c,0x3f] as IID_IIterable_1_Windows_Devices_HumanInterfaceDevice_HidBooleanControl }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControl> => [0x5cde3c23,0xd054,0x53d6,0xab,0xf1,0x41,0xe7,0x33,0x79,0xb4,0x72] as IID_IIterator_1_Windows_Devices_HumanInterfaceDevice_HidBooleanControl }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::input::PointerDevice> => [0xcf5674f1,0x9808,0x5a2b,0x80,0xb8,0x56,0x84,0xed,0x0e,0xa8,0x16] as IID_IVectorView_1_Windows_Devices_Input_PointerDevice }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::input::PointerDevice> => [0xad26662c,0x845b,0x5c6d,0xae,0xaa,0x40,0x6f,0x48,0xc2,0x1a,0xe9] as IID_IIterable_1_Windows_Devices_Input_PointerDevice }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::input::PointerDevice> => [0xde94641c,0x7960,0x5fcd,0xab,0xe8,0xd6,0xba,0x60,0x9e,0xf7,0xd3] as IID_IIterator_1_Windows_Devices_Input_PointerDevice }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::input::PointerDeviceUsage> => [0x8e5a2c7e,0x3830,0x50d5,0x92,0xba,0x31,0x63,0xc8,0x9c,0xbb,0xd0] as IID_IVectorView_1_Windows_Devices_Input_PointerDeviceUsage }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::input::PointerDeviceUsage> => [0x592d6618,0xeaab,0x5a79,0xa4,0x7a,0xc7,0xfc,0x0b,0x74,0x9a,0x4e] as IID_IIterable_1_Windows_Devices_Input_PointerDeviceUsage }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::input::PointerDeviceUsage> => [0x9ab2160d,0x11ef,0x5eca,0x8d,0xd9,0x3e,0x13,0xaa,0x4e,0x5f,0x99] as IID_IIterator_1_Windows_Devices_Input_PointerDeviceUsage }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::perception::provider::PerceptionCorrelation> => [0xca6bf87e,0x1745,0x5cd0,0xae,0xe2,0x59,0x73,0x6f,0x5a,0x20,0x6d] as IID_IIterable_1_Windows_Devices_Perception_Provider_PerceptionCorrelation }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::perception::provider::PerceptionCorrelation> => [0xc4db1093,0xd705,0x5503,0x8b,0xce,0x68,0x53,0x5c,0xd4,0x2f,0xfa] as IID_IIterator_1_Windows_Devices_Perception_Provider_PerceptionCorrelation }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::perception::provider::PerceptionCorrelation> => [0x244cad66,0xafbe,0x5394,0xb7,0xb7,0x43,0xa6,0x1f,0xcb,0xfc,0x6d] as IID_IVectorView_1_Windows_Devices_Perception_Provider_PerceptionCorrelation }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource> => [0xf272ae7a,0xc5c4,0x5712,0x85,0x52,0x01,0xde,0xb8,0xb7,0x0e,0x07] as IID_IVectorView_1_Windows_Devices_Perception_PerceptionInfraredFrameSource }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource> => [0x9309d0db,0x338d,0x5adf,0x8b,0x3e,0x50,0x9b,0xfd,0xfc,0xce,0xf3] as IID_IIterable_1_Windows_Devices_Perception_PerceptionInfraredFrameSource }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::perception::PerceptionInfraredFrameSource> => [0xb22b294f,0xa4d2,0x5726,0xa7,0xfc,0x5e,0x33,0x14,0x32,0xd9,0xb4] as IID_IIterator_1_Windows_Devices_Perception_PerceptionInfraredFrameSource }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource> => [0x574d3642,0x9f78,0x5125,0x85,0x1f,0x8b,0x67,0xe0,0x31,0x3e,0x2f] as IID_IVectorView_1_Windows_Devices_Perception_PerceptionDepthFrameSource }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource> => [0x8a07e1e8,0x5a02,0x585b,0xa2,0x6e,0xad,0x79,0xbe,0xaa,0x94,0xcf] as IID_IIterable_1_Windows_Devices_Perception_PerceptionDepthFrameSource }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::perception::PerceptionDepthFrameSource> => [0x20cff8c2,0x7844,0x54e5,0xae,0x4f,0x57,0xe7,0x76,0x8f,0x9b,0x69] as IID_IIterator_1_Windows_Devices_Perception_PerceptionDepthFrameSource }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource> => [0xcbf2f698,0x31a0,0x53c7,0x92,0x7b,0x8e,0x16,0x75,0xf7,0x43,0xbc] as IID_IVectorView_1_Windows_Devices_Perception_PerceptionColorFrameSource }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource> => [0xdb18069e,0x7b5a,0x54c3,0xa6,0x27,0xd5,0x6f,0x95,0x17,0xfd,0xf5] as IID_IIterable_1_Windows_Devices_Perception_PerceptionColorFrameSource }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::perception::PerceptionColorFrameSource> => [0x24089f00,0xba6d,0x50d4,0xac,0x46,0xf2,0x88,0x75,0x5e,0x41,0x81] as IID_IIterator_1_Windows_Devices_Perception_PerceptionColorFrameSource }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::perception::PerceptionVideoProfile> => [0x8dcb30e8,0x4ec1,0x51b7,0x99,0x97,0x10,0xf3,0x25,0xf0,0x3d,0x47] as IID_IVectorView_1_Windows_Devices_Perception_PerceptionVideoProfile }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::perception::PerceptionVideoProfile> => [0xf6aea351,0xeb9b,0x564d,0xb1,0x0a,0x06,0x67,0x30,0x94,0xac,0xc8] as IID_IIterable_1_Windows_Devices_Perception_PerceptionVideoProfile }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::perception::PerceptionVideoProfile> => [0x38ce8062,0x7079,0x5d7b,0x84,0x1f,0x9a,0xa4,0x58,0x0f,0xd5,0xf1] as IID_IIterator_1_Windows_Devices_Perception_PerceptionVideoProfile }
		RT_PINTERFACE!{ for IVectorView<u32> => [0xe5ce1a07,0x8d33,0x5007,0xba,0x64,0x7d,0x25,0x08,0xcc,0xf8,0x5c] as IID_IVectorView_1_System_UInt32 }
		RT_PINTERFACE!{ for IIterable<u32> => [0x421d4b91,0xb13b,0x5f37,0xae,0x54,0xb5,0x24,0x9b,0xd8,0x05,0x39] as IID_IIterable_1_System_UInt32 }
		RT_PINTERFACE!{ for IIterator<u32> => [0xf06a2739,0x9443,0x5ef0,0xb2,0x84,0xdc,0x5a,0xff,0x3e,0x7d,0x10] as IID_IIterator_1_System_UInt32 }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::pointofservice::PosPrinterRotation> => [0x6f694309,0x1082,0x5d84,0xa5,0xea,0x2f,0xae,0xd6,0xb6,0x59,0x0e] as IID_IVectorView_1_Windows_Devices_PointOfService_PosPrinterRotation }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::pointofservice::PosPrinterRotation> => [0x1b1e4d8d,0x15f5,0x5802,0x9b,0x23,0x8b,0x75,0xce,0x2a,0x58,0xc5] as IID_IIterable_1_Windows_Devices_PointOfService_PosPrinterRotation }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::pointofservice::PosPrinterRotation> => [0xf687ef7c,0xb11e,0x56c3,0x91,0x58,0xe6,0xba,0x15,0xbd,0x52,0x1b] as IID_IIterator_1_Windows_Devices_PointOfService_PosPrinterRotation }
		RT_PINTERFACE!{ for<'a> IMap<&'a str, &'a ::rt::gen::windows::graphics::imaging::BitmapTypedValue> => [0x2c70ef8d,0x5d4c,0x5185,0x8d,0xb7,0xfe,0xd8,0x77,0x28,0x16,0x5d] as IID_IMap_2_System_String_Windows_Graphics_Imaging_BitmapTypedValue }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<&'a str, &'a ::rt::gen::windows::graphics::imaging::BitmapTypedValue>> => [0x05f9430c,0x2f22,0x5638,0xaa,0x89,0x8c,0x9a,0xbc,0xd5,0x4f,0xf9] as IID_IIterable_1_Windows_Foundation_Collections_IKeyValuePair_2_System_String_Windows_Graphics_Imaging_BitmapTypedValue }
		RT_PINTERFACE!{ for<'a> IKeyValuePair<&'a str, &'a ::rt::gen::windows::graphics::imaging::BitmapTypedValue> => [0x93621bf0,0xdae9,0x5f00,0x94,0xac,0x79,0x5a,0xa9,0x43,0xdc,0xa6] as IID_IKeyValuePair_2_System_String_Windows_Graphics_Imaging_BitmapTypedValue }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<&'a str, &'a ::rt::gen::windows::graphics::imaging::BitmapTypedValue>> => [0x2ad3fb0c,0x0656,0x5302,0xb5,0x04,0x31,0x53,0xbe,0x84,0x51,0x61] as IID_IIterator_1_Windows_Foundation_Collections_IKeyValuePair_2_System_String_Windows_Graphics_Imaging_BitmapTypedValue }
		RT_PINTERFACE!{ for<'a> IMapView<&'a str, &'a ::rt::gen::windows::graphics::imaging::BitmapTypedValue> => [0x9cda5a9a,0x8924,0x5b3b,0x8b,0x19,0x89,0x4d,0x8d,0xa9,0x9d,0xde] as IID_IMapView_2_System_String_Windows_Graphics_Imaging_BitmapTypedValue }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::radios::Radio> => [0x65066c36,0x090b,0x5466,0xb8,0xe5,0xe7,0x56,0x5d,0xc3,0x41,0x75] as IID_IVectorView_1_Windows_Devices_Radios_Radio }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::radios::Radio> => [0xe82500af,0x1f53,0x504e,0xb8,0xbe,0xda,0xc4,0xfb,0xb6,0x90,0x84] as IID_IIterable_1_Windows_Devices_Radios_Radio }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::radios::Radio> => [0xcf37ede7,0xeaec,0x5b8f,0xad,0x31,0x4d,0x51,0xab,0xd9,0xdb,0x05] as IID_IIterator_1_Windows_Devices_Radios_Radio }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReading> => [0x726234a9,0x7137,0x55c4,0xa4,0x44,0x35,0x2d,0xcd,0xc3,0x0d,0x77] as IID_IVectorView_1_Windows_Devices_Sensors_ActivitySensorReading }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReading> => [0x9a34ce03,0x8c6d,0x5994,0x90,0x7f,0xd5,0xc2,0xd1,0x91,0x48,0xcb] as IID_IIterable_1_Windows_Devices_Sensors_ActivitySensorReading }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReading> => [0xd2dab535,0x0c94,0x547e,0xaf,0xe3,0x55,0x27,0xbc,0xbe,0xb9,0xcc] as IID_IIterator_1_Windows_Devices_Sensors_ActivitySensorReading }
		RT_PINTERFACE!{ for IVector<::rt::gen::windows::devices::sensors::ActivityType> => [0xe3e660d6,0xd041,0x5ecd,0xb1,0x8b,0xfa,0x25,0x4e,0x4a,0x86,0x0f] as IID_IVector_1_Windows_Devices_Sensors_ActivityType }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::sensors::ActivityType> => [0x2a04cdfa,0x5dfd,0x5178,0x87,0x31,0xad,0xe9,0x98,0xe4,0xa7,0xf6] as IID_IIterable_1_Windows_Devices_Sensors_ActivityType }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::sensors::ActivityType> => [0x40524281,0xa7c6,0x50b1,0xb6,0xf5,0x0b,0xaa,0x95,0xd9,0x02,0xc2] as IID_IIterator_1_Windows_Devices_Sensors_ActivityType }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::sensors::ActivityType> => [0xfc7a0488,0x2803,0x505c,0x9e,0x62,0x92,0x00,0xaf,0xe4,0x16,0xc6] as IID_IVectorView_1_Windows_Devices_Sensors_ActivityType }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReadingChangeReport> => [0x10e48a80,0xdd6a,0x5704,0x8f,0x3d,0x3d,0x46,0x11,0x1f,0x39,0x1e] as IID_IVectorView_1_Windows_Devices_Sensors_ActivitySensorReadingChangeReport }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReadingChangeReport> => [0x551a4962,0x9e96,0x5e6b,0x8b,0x8a,0x65,0xee,0x3d,0x00,0x46,0xf3] as IID_IIterable_1_Windows_Devices_Sensors_ActivitySensorReadingChangeReport }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::sensors::ActivitySensorReadingChangeReport> => [0x9c07034e,0x8333,0x59d5,0x8d,0x60,0x0e,0x3f,0x04,0x38,0xac,0x12] as IID_IIterator_1_Windows_Devices_Sensors_ActivitySensorReadingChangeReport }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::sensors::PedometerReading> => [0x52076f5c,0x7838,0x54d9,0x95,0x17,0x55,0x11,0xeb,0x62,0x79,0x52] as IID_IVectorView_1_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::sensors::PedometerReading> => [0xbbb61a5c,0x98c3,0x5718,0x88,0xfe,0x53,0x92,0xa7,0x45,0x1e,0x2d] as IID_IIterable_1_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::sensors::PedometerReading> => [0x0ac70ed3,0x8553,0x5ef3,0x92,0xf8,0x43,0x86,0x09,0x62,0x30,0x87] as IID_IIterator_1_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> IMapView<::rt::gen::windows::devices::sensors::PedometerStepKind, &'a ::rt::gen::windows::devices::sensors::PedometerReading> => [0x64f0c54c,0x4865,0x56bd,0xac,0x98,0x64,0xa9,0x84,0x51,0xe3,0x62] as IID_IMapView_2_Windows_Devices_Sensors_PedometerStepKind_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<::rt::gen::windows::devices::sensors::PedometerStepKind, &'a ::rt::gen::windows::devices::sensors::PedometerReading>> => [0x098f29cb,0xbc91,0x5639,0xa5,0x41,0xd5,0xa4,0x81,0x1e,0xc3,0x5b] as IID_IIterable_1_Windows_Foundation_Collections_IKeyValuePair_2_Windows_Devices_Sensors_PedometerStepKind_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> IKeyValuePair<::rt::gen::windows::devices::sensors::PedometerStepKind, &'a ::rt::gen::windows::devices::sensors::PedometerReading> => [0xb270d3b8,0x3dd2,0x599f,0xa6,0x71,0x2d,0xe5,0x03,0x55,0x03,0xda] as IID_IKeyValuePair_2_Windows_Devices_Sensors_PedometerStepKind_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::foundation::collections::IKeyValuePair<::rt::gen::windows::devices::sensors::PedometerStepKind, &'a ::rt::gen::windows::devices::sensors::PedometerReading>> => [0x3e88fe66,0xd4a7,0x5658,0xb5,0xcf,0x1a,0x39,0xe1,0xfc,0x41,0x65] as IID_IIterator_1_Windows_Foundation_Collections_IKeyValuePair_2_Windows_Devices_Sensors_PedometerStepKind_Windows_Devices_Sensors_PedometerReading }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::sensors::ProximitySensorReading> => [0x7a09d76c,0x8ced,0x5e30,0xb7,0xfe,0x1f,0xf7,0x4d,0x4d,0x98,0x14] as IID_IVectorView_1_Windows_Devices_Sensors_ProximitySensorReading }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::sensors::ProximitySensorReading> => [0x301ebccf,0x11ab,0x5e90,0x98,0xee,0xbd,0x99,0xc0,0xe3,0xbb,0x76] as IID_IIterable_1_Windows_Devices_Sensors_ProximitySensorReading }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::sensors::ProximitySensorReading> => [0x1d4f08df,0x7f49,0x573b,0x93,0x6a,0x6d,0x4d,0x4e,0x61,0x09,0x30] as IID_IIterator_1_Windows_Devices_Sensors_ProximitySensorReading }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::smartcards::SmartCard> => [0x4bee6991,0x3508,0x5f03,0xa2,0xf4,0x90,0xa5,0xdd,0xb2,0x6b,0xd8] as IID_IVectorView_1_Windows_Devices_SmartCards_SmartCard }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::smartcards::SmartCard> => [0xa32c5202,0xd113,0x535f,0x88,0x0e,0x50,0xf3,0xe5,0x12,0x1e,0xf8] as IID_IIterable_1_Windows_Devices_SmartCards_SmartCard }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::smartcards::SmartCard> => [0x86b29903,0x916e,0x5817,0xbc,0x96,0xdf,0x32,0x44,0x75,0xe3,0x1a] as IID_IIterator_1_Windows_Devices_SmartCards_SmartCard }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbInterface> => [0x9c69ec7f,0x2e42,0x58cd,0xa7,0x4a,0xf4,0x97,0x48,0x11,0x13,0x4d] as IID_IVectorView_1_Windows_Devices_Usb_UsbInterface }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbInterface> => [0xf54037ed,0x92e9,0x590d,0xb9,0x04,0x3a,0xd7,0xbf,0xa9,0xa6,0x21] as IID_IIterable_1_Windows_Devices_Usb_UsbInterface }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbInterface> => [0x216b5a5f,0x63e3,0x5a9b,0x9c,0x99,0xb0,0x9c,0xbc,0x0f,0xf3,0xb1] as IID_IIterator_1_Windows_Devices_Usb_UsbInterface }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbDescriptor> => [0x5408baa2,0x291e,0x537a,0xb6,0x1f,0x13,0x70,0x62,0xf7,0xff,0x7d] as IID_IVectorView_1_Windows_Devices_Usb_UsbDescriptor }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbDescriptor> => [0x989909a5,0x5a03,0x51fb,0xbd,0x94,0x84,0xda,0x7b,0xda,0x88,0x19] as IID_IIterable_1_Windows_Devices_Usb_UsbDescriptor }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbDescriptor> => [0x521598ed,0x0167,0x528e,0x99,0x0d,0x52,0xab,0xb7,0x12,0xf0,0x72] as IID_IIterator_1_Windows_Devices_Usb_UsbDescriptor }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbBulkInPipe> => [0xa93c84bc,0x6484,0x5959,0xb6,0x1a,0x70,0x3c,0xc7,0x11,0x5f,0x6f] as IID_IVectorView_1_Windows_Devices_Usb_UsbBulkInPipe }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbBulkInPipe> => [0x2201a671,0x42d2,0x508d,0xa8,0x48,0x64,0xb5,0x44,0x70,0x83,0xc8] as IID_IIterable_1_Windows_Devices_Usb_UsbBulkInPipe }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbBulkInPipe> => [0xd7af2c5b,0x528d,0x5cbb,0xa9,0x97,0xd8,0x30,0xad,0xe7,0x04,0xc7] as IID_IIterator_1_Windows_Devices_Usb_UsbBulkInPipe }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbInterruptInPipe> => [0x37469574,0xb4c5,0x5ba0,0x96,0x16,0x89,0x4d,0xd8,0x22,0xff,0x5b] as IID_IVectorView_1_Windows_Devices_Usb_UsbInterruptInPipe }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbInterruptInPipe> => [0x39aef336,0x18aa,0x5be4,0x86,0xd9,0xe3,0x32,0xfe,0x26,0x32,0xf3] as IID_IIterable_1_Windows_Devices_Usb_UsbInterruptInPipe }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbInterruptInPipe> => [0xe3a7b1c0,0x74f6,0x5292,0xa2,0x2a,0x67,0x2a,0xa2,0xb4,0x99,0x85] as IID_IIterator_1_Windows_Devices_Usb_UsbInterruptInPipe }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbBulkOutPipe> => [0x0a873512,0x15f1,0x5e8e,0xa7,0x2a,0x04,0x5c,0xfd,0x7a,0x5e,0x83] as IID_IVectorView_1_Windows_Devices_Usb_UsbBulkOutPipe }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbBulkOutPipe> => [0x9824caba,0x5ca6,0x5c2d,0x80,0xcf,0x19,0x49,0x02,0x6d,0x78,0x57] as IID_IIterable_1_Windows_Devices_Usb_UsbBulkOutPipe }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbBulkOutPipe> => [0x46dd2f6a,0x573b,0x5c45,0xb1,0x68,0x92,0x23,0x03,0x84,0x91,0xdd] as IID_IIterator_1_Windows_Devices_Usb_UsbBulkOutPipe }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbInterruptOutPipe> => [0x748196c8,0x83bf,0x5ec3,0x8d,0x28,0xa3,0x11,0x2b,0x3e,0xe3,0xcc] as IID_IVectorView_1_Windows_Devices_Usb_UsbInterruptOutPipe }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbInterruptOutPipe> => [0xe61a011e,0x4abe,0x53f2,0x83,0xb3,0xed,0x4a,0x94,0x9d,0x2e,0x3f] as IID_IIterable_1_Windows_Devices_Usb_UsbInterruptOutPipe }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbInterruptOutPipe> => [0xcbd8d8a8,0x2286,0x5cbd,0xa6,0xe4,0x96,0x27,0x42,0xff,0xd9,0x1a] as IID_IIterator_1_Windows_Devices_Usb_UsbInterruptOutPipe }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbInterfaceSetting> => [0x71194af7,0x77c2,0x54d5,0xa1,0x16,0x28,0x7f,0x0b,0x7f,0xd5,0x3f] as IID_IVectorView_1_Windows_Devices_Usb_UsbInterfaceSetting }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbInterfaceSetting> => [0x1aaf5739,0x9c2c,0x533e,0xa0,0xe9,0xd5,0x3f,0xdb,0x45,0xd1,0x5d] as IID_IIterable_1_Windows_Devices_Usb_UsbInterfaceSetting }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbInterfaceSetting> => [0x71267ec7,0x5697,0x5dea,0xb2,0xf8,0x14,0xcf,0x69,0x8e,0xc0,0xad] as IID_IIterator_1_Windows_Devices_Usb_UsbInterfaceSetting }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbBulkInEndpointDescriptor> => [0x9c69ac78,0x309e,0x5763,0xaf,0x26,0x97,0x06,0xff,0xa4,0x7e,0xc0] as IID_IVectorView_1_Windows_Devices_Usb_UsbBulkInEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbBulkInEndpointDescriptor> => [0x101b1fd9,0xf1c9,0x5dda,0x9a,0xd4,0x71,0x17,0x6f,0xa8,0x39,0xb2] as IID_IIterable_1_Windows_Devices_Usb_UsbBulkInEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbBulkInEndpointDescriptor> => [0xea511030,0x89c4,0x503d,0x8c,0xaf,0x66,0x7f,0x42,0x30,0xd2,0xa9] as IID_IIterator_1_Windows_Devices_Usb_UsbBulkInEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbInterruptInEndpointDescriptor> => [0x3fc7f890,0x218e,0x5057,0x90,0x4d,0x63,0x87,0xc5,0x91,0xcc,0x93] as IID_IVectorView_1_Windows_Devices_Usb_UsbInterruptInEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbInterruptInEndpointDescriptor> => [0x8a7bac69,0x1f10,0x59c7,0x98,0x37,0x72,0xcf,0xed,0x71,0x54,0xa4] as IID_IIterable_1_Windows_Devices_Usb_UsbInterruptInEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbInterruptInEndpointDescriptor> => [0x6717500f,0xec1c,0x5b12,0xbf,0x33,0x0e,0x3e,0x3d,0x24,0x45,0x87] as IID_IIterator_1_Windows_Devices_Usb_UsbInterruptInEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbBulkOutEndpointDescriptor> => [0x22a53676,0xa3ea,0x5dcd,0xbb,0x39,0xb2,0x8a,0x53,0x27,0xc4,0xa3] as IID_IVectorView_1_Windows_Devices_Usb_UsbBulkOutEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbBulkOutEndpointDescriptor> => [0xb80beb39,0x62b3,0x5f59,0xb3,0xe7,0x88,0x2c,0xc9,0xc5,0xb0,0xc0] as IID_IIterable_1_Windows_Devices_Usb_UsbBulkOutEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbBulkOutEndpointDescriptor> => [0xa8b89ab3,0x883d,0x5361,0x99,0x03,0xf4,0x89,0xcc,0x62,0xbe,0xa5] as IID_IIterator_1_Windows_Devices_Usb_UsbBulkOutEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::usb::UsbInterruptOutEndpointDescriptor> => [0x984e7e15,0xc5ac,0x5140,0xa3,0xc0,0xb5,0x83,0x19,0x00,0x85,0xd7] as IID_IVectorView_1_Windows_Devices_Usb_UsbInterruptOutEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::usb::UsbInterruptOutEndpointDescriptor> => [0x09393d62,0x2316,0x536b,0x8a,0x10,0x70,0x38,0x88,0x4a,0xb2,0xa7] as IID_IIterable_1_Windows_Devices_Usb_UsbInterruptOutEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::usb::UsbInterruptOutEndpointDescriptor> => [0x4b6426db,0xdb32,0x5b51,0xad,0xad,0x04,0x53,0x2e,0xa9,0x4a,0xcd] as IID_IIterator_1_Windows_Devices_Usb_UsbInterruptOutEndpointDescriptor }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::wifi::WiFiAdapter> => [0x670a3c41,0xecc8,0x55c2,0x84,0xd4,0x51,0x86,0x44,0x96,0xa3,0x28] as IID_IVectorView_1_Windows_Devices_WiFi_WiFiAdapter }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::wifi::WiFiAdapter> => [0xe0bc76c4,0x8d0c,0x53fc,0xbc,0xd4,0x22,0x8f,0x47,0x21,0x0a,0xce] as IID_IIterable_1_Windows_Devices_WiFi_WiFiAdapter }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::wifi::WiFiAdapter> => [0x144136c6,0xb502,0x5a52,0x90,0xfc,0x22,0xa0,0x93,0x18,0xf9,0x32] as IID_IIterator_1_Windows_Devices_WiFi_WiFiAdapter }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::wifi::WiFiAvailableNetwork> => [0x7c65d286,0x7285,0x5d63,0xbd,0xea,0x5e,0xf9,0x51,0xbd,0xf6,0x18] as IID_IVectorView_1_Windows_Devices_WiFi_WiFiAvailableNetwork }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::wifi::WiFiAvailableNetwork> => [0xf17484ea,0xc71e,0x5d3e,0xb7,0x4c,0x3a,0x0e,0x61,0xdd,0x9c,0x20] as IID_IIterable_1_Windows_Devices_WiFi_WiFiAvailableNetwork }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::wifi::WiFiAvailableNetwork> => [0x468677c4,0xebb9,0x5196,0x83,0x6d,0x72,0xfa,0xa9,0xfe,0x67,0x3e] as IID_IIterator_1_Windows_Devices_WiFi_WiFiAvailableNetwork }
		RT_PINTERFACE!{ for<'a> IVector<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectInformationElement> => [0xb8c55492,0xe4de,0x5ba7,0x84,0x76,0xd3,0xba,0xb5,0x57,0xcd,0xd6] as IID_IVector_1_Windows_Devices_WiFiDirect_WiFiDirectInformationElement }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectInformationElement> => [0x19c1ca4e,0x9561,0x5253,0x96,0xd9,0xdb,0xaf,0x28,0xd4,0x7d,0x89] as IID_IIterable_1_Windows_Devices_WiFiDirect_WiFiDirectInformationElement }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectInformationElement> => [0xcf806026,0xc915,0x553e,0xaf,0x3c,0x8d,0xa4,0x38,0x71,0xb6,0x93] as IID_IIterator_1_Windows_Devices_WiFiDirect_WiFiDirectInformationElement }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::wifidirect::WiFiDirectInformationElement> => [0x6dcffadb,0x04c5,0x535e,0xad,0xd4,0x13,0x89,0xb3,0xbe,0x6e,0xca] as IID_IVectorView_1_Windows_Devices_WiFiDirect_WiFiDirectInformationElement }
		RT_PINTERFACE!{ for IVector<::rt::gen::windows::devices::wifidirect::WiFiDirectConfigurationMethod> => [0x9b498bc0,0xb474,0x5587,0xb6,0x5c,0xe6,0x00,0x96,0x5f,0x8f,0xd0] as IID_IVector_1_Windows_Devices_WiFiDirect_WiFiDirectConfigurationMethod }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::wifidirect::WiFiDirectConfigurationMethod> => [0x794f12da,0x2dc6,0x5277,0x82,0xdc,0xb0,0x78,0x16,0x10,0x53,0x7b] as IID_IIterable_1_Windows_Devices_WiFiDirect_WiFiDirectConfigurationMethod }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::wifidirect::WiFiDirectConfigurationMethod> => [0x201940f9,0xa368,0x57f4,0x9e,0xf2,0x3f,0x64,0xe2,0x43,0xe0,0xa4] as IID_IIterator_1_Windows_Devices_WiFiDirect_WiFiDirectConfigurationMethod }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::wifidirect::WiFiDirectConfigurationMethod> => [0x61a32670,0x04d3,0x551d,0xad,0x66,0xbd,0x04,0xe9,0xef,0x5c,0x78] as IID_IVectorView_1_Windows_Devices_WiFiDirect_WiFiDirectConfigurationMethod }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::networking::EndpointPair> => [0x8780a851,0x6d48,0x5006,0x92,0x88,0x81,0xf3,0xd7,0x04,0x5a,0x96] as IID_IVectorView_1_Windows_Networking_EndpointPair }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::networking::EndpointPair> => [0xd7ec83c4,0xa17b,0x51bf,0x89,0x97,0xaa,0x33,0xb9,0x10,0x2d,0xc9] as IID_IIterable_1_Windows_Networking_EndpointPair }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::networking::EndpointPair> => [0xc899ff9f,0xe6f5,0x5673,0x81,0x0c,0x04,0xe2,0xff,0x98,0x70,0x4f] as IID_IIterator_1_Windows_Networking_EndpointPair }
		RT_PINTERFACE!{ for IVector<::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceConfigurationMethod> => [0xf6a6f91c,0x0579,0x565d,0xbe,0x07,0x45,0x38,0xa5,0x56,0x90,0xbe] as IID_IVector_1_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceConfigurationMethod }
		RT_PINTERFACE!{ for IIterable<::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceConfigurationMethod> => [0xd9773b1a,0xa148,0x58bf,0x9c,0x4b,0xaf,0xea,0xc9,0xbe,0x3a,0xb4] as IID_IIterable_1_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceConfigurationMethod }
		RT_PINTERFACE!{ for IIterator<::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceConfigurationMethod> => [0x19889f5e,0x49ae,0x5e31,0xb0,0x59,0x08,0x3f,0x9f,0x15,0x32,0xc3] as IID_IIterator_1_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceConfigurationMethod }
		RT_PINTERFACE!{ for IVectorView<::rt::gen::windows::devices::wifidirect::services::WiFiDirectServiceConfigurationMethod> => [0xdc710fe1,0x7f04,0x515b,0x8a,0xc1,0x1c,0x5d,0x3c,0x0d,0x2b,0x28] as IID_IVectorView_1_Windows_Devices_WiFiDirect_Services_WiFiDirectServiceConfigurationMethod }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic> => [0xcb3ab3ae,0xb561,0x504f,0xa8,0x08,0x59,0x9d,0xec,0xeb,0x2d,0xf4] as IID_IVectorView_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattCharacteristic }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic> => [0xe3c56728,0x7f2d,0x5a0d,0xad,0x38,0x03,0x0d,0x39,0xc6,0x0f,0x9f] as IID_IIterable_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattCharacteristic }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::bluetooth::genericattributeprofile::GattCharacteristic> => [0x1ffc4777,0x4346,0x5564,0xb7,0xa5,0x59,0xea,0xe3,0x85,0xf4,0xf6] as IID_IIterator_1_Windows_Devices_Bluetooth_GenericAttributeProfile_GattCharacteristic }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControlDescription> => [0xaab72786,0xec34,0x536f,0xa7,0xc5,0x27,0x39,0x47,0x53,0xdf,0x2c] as IID_IVectorView_1_Windows_Devices_HumanInterfaceDevice_HidBooleanControlDescription }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControlDescription> => [0xd0ff0fed,0xa156,0x58bf,0x94,0x11,0x57,0x77,0xdf,0x9d,0x57,0xbf] as IID_IIterable_1_Windows_Devices_HumanInterfaceDevice_HidBooleanControlDescription }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidBooleanControlDescription> => [0x203203b0,0xb7f4,0x542d,0xb0,0xd0,0x9c,0xaa,0x1f,0xb5,0x5d,0x7f] as IID_IIterator_1_Windows_Devices_HumanInterfaceDevice_HidBooleanControlDescription }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControlDescription> => [0xe02ca66c,0x610a,0x51b4,0xae,0xf9,0x37,0x07,0xb6,0x97,0xb9,0x85] as IID_IVectorView_1_Windows_Devices_HumanInterfaceDevice_HidNumericControlDescription }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControlDescription> => [0x868f060d,0xe0d4,0x571b,0xb2,0xf7,0x43,0x1d,0x69,0x84,0xa5,0x13] as IID_IIterable_1_Windows_Devices_HumanInterfaceDevice_HidNumericControlDescription }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::humaninterfacedevice::HidNumericControlDescription> => [0x52b9c36e,0x7d95,0x5d1c,0xac,0xab,0x23,0xc1,0x9e,0xa7,0x6f,0x01] as IID_IIterator_1_Windows_Devices_HumanInterfaceDevice_HidNumericControlDescription }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::sms::ISmsBinaryMessage> => [0x6ea176ea,0x99ea,0x5c79,0x87,0x6a,0xf4,0xc4,0x37,0xb8,0x3d,0xf6] as IID_IVectorView_1_Windows_Devices_Sms_ISmsBinaryMessage }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::sms::ISmsBinaryMessage> => [0x5678a6a5,0x4d5a,0x51c2,0xa1,0x33,0x4b,0x83,0xbf,0x25,0xd9,0x87] as IID_IIterable_1_Windows_Devices_Sms_ISmsBinaryMessage }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::sms::ISmsBinaryMessage> => [0x13e60d89,0xea0a,0x5b01,0x9c,0x2f,0x0e,0x5b,0x43,0x50,0x58,0xe0] as IID_IIterator_1_Windows_Devices_Sms_ISmsBinaryMessage }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::devices::sms::ISmsMessage> => [0xd3acc5b1,0x6f85,0x507e,0xb4,0x0a,0x69,0x50,0x74,0x9b,0x42,0x6f] as IID_IVectorView_1_Windows_Devices_Sms_ISmsMessage }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::devices::sms::ISmsMessage> => [0xecabfd70,0x9601,0x5e38,0x83,0xcf,0xb1,0x04,0x60,0x22,0xa2,0x44] as IID_IIterable_1_Windows_Devices_Sms_ISmsMessage }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::devices::sms::ISmsMessage> => [0xeed04f5c,0xb2b2,0x5c83,0x8b,0x13,0xc7,0x8a,0xf6,0xca,0x3a,0x18] as IID_IIterator_1_Windows_Devices_Sms_ISmsMessage }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::storage::StorageFile> => [0x80646519,0x5e2a,0x595d,0xa8,0xcd,0x2a,0x24,0xb4,0x06,0x7f,0x1b] as IID_IVectorView_1_Windows_Storage_StorageFile }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::storage::StorageFile> => [0x9ac00304,0x83ea,0x5688,0x87,0xb6,0xae,0x38,0xaa,0xb6,0x5d,0x0b] as IID_IIterable_1_Windows_Storage_StorageFile }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::storage::StorageFile> => [0x43e29f53,0x0298,0x55aa,0xa6,0xc8,0x4e,0xdd,0x32,0x3d,0x95,0x98] as IID_IIterator_1_Windows_Storage_StorageFile }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::storage::StorageFolder> => [0xe20debc6,0xdc4e,0x542e,0xa2,0xe7,0xa2,0x4d,0x19,0xc8,0xdd,0x62] as IID_IVectorView_1_Windows_Storage_StorageFolder }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::storage::StorageFolder> => [0x4669befc,0xae5c,0x52b1,0x8a,0x97,0x54,0x66,0xce,0x61,0xe9,0x4e] as IID_IIterable_1_Windows_Storage_StorageFolder }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::storage::StorageFolder> => [0x5aac96fb,0xb3b9,0x5a7f,0xa9,0x20,0x4b,0x5a,0x8d,0xf8,0x11,0x68] as IID_IIterator_1_Windows_Storage_StorageFolder }
		RT_PINTERFACE!{ for<'a> IVectorView<&'a ::rt::gen::windows::storage::IStorageItem> => [0x85575a41,0x06cb,0x58d0,0xb9,0x8a,0x7c,0x8f,0x06,0xe6,0xe9,0xd7] as IID_IVectorView_1_Windows_Storage_IStorageItem }
		RT_PINTERFACE!{ for<'a> IIterable<&'a ::rt::gen::windows::storage::IStorageItem> => [0xbb8b8418,0x65d1,0x544b,0xb0,0x83,0x6d,0x17,0x2f,0x56,0x8c,0x73] as IID_IIterable_1_Windows_Storage_IStorageItem }
		RT_PINTERFACE!{ for<'a> IIterator<&'a ::rt::gen::windows::storage::IStorageItem> => [0x05b487c2,0x3830,0x5d3c,0x98,0xda,0x25,0xfa,0x11,0x54,0x2d,0xbd] as IID_IIterator_1_Windows_Storage_IStorageItem }
} // Windows.Foundation.Collections
pub mod metadata { // Windows.Foundation.Metadata
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum GCPressureAmount: i32 {
			Low (GCPressureAmount_Low) = 0, Medium (GCPressureAmount_Medium) = 1, High (GCPressureAmount_High) = 2,
		}}
		DEFINE_IID!(IID_IApiInformationStatics, 2574531070, 63105, 18961, 180, 22, 193, 58, 71, 232, 186, 54);
		RT_INTERFACE!{interface IApiInformationStatics(IApiInformationStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IApiInformationStatics] {
			fn IsTypePresent(&mut self, typeName: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsMethodPresent(&mut self, typeName: ::w::HSTRING, methodName: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsMethodPresentWithArity(&mut self, typeName: ::w::HSTRING, methodName: ::w::HSTRING, inputParameterCount: u32, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsEventPresent(&mut self, typeName: ::w::HSTRING, eventName: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsPropertyPresent(&mut self, typeName: ::w::HSTRING, propertyName: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsReadOnlyPropertyPresent(&mut self, typeName: ::w::HSTRING, propertyName: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsWriteablePropertyPresent(&mut self, typeName: ::w::HSTRING, propertyName: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsEnumNamedValuePresent(&mut self, enumTypeName: ::w::HSTRING, valueName: ::w::HSTRING, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsApiContractPresentByMajor(&mut self, contractName: ::w::HSTRING, majorVersion: u16, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsApiContractPresentByMajorAndMinor(&mut self, contractName: ::w::HSTRING, majorVersion: u16, minorVersion: u16, out: *mut ::w::BOOL) -> ::w::HRESULT
		}}
		RT_ENUM! { enum Platform: i32 {
			Windows (Platform_Windows) = 0, WindowsPhone (Platform_WindowsPhone) = 1,
		}}
		RT_ENUM! { enum AttributeTargets: u32 {
			All (AttributeTargets_All) = 4294967295, Delegate (AttributeTargets_Delegate) = 1, Enum (AttributeTargets_Enum) = 2, Event (AttributeTargets_Event) = 4, Field (AttributeTargets_Field) = 8, Interface (AttributeTargets_Interface) = 16, Method (AttributeTargets_Method) = 64, Parameter (AttributeTargets_Parameter) = 128, Property (AttributeTargets_Property) = 256, RuntimeClass (AttributeTargets_RuntimeClass) = 512, Struct (AttributeTargets_Struct) = 1024, InterfaceImpl (AttributeTargets_InterfaceImpl) = 2048, ApiContract (AttributeTargets_ApiContract) = 8192,
		}}
		RT_ENUM! { enum CompositionType: i32 {
			Protected (CompositionType_Protected) = 1, Public (CompositionType_Public) = 2,
		}}
		RT_ENUM! { enum ThreadingModel: i32 {
			STA (ThreadingModel_STA) = 1, MTA (ThreadingModel_MTA) = 2, Both (ThreadingModel_Both) = 3, InvalidThreading (ThreadingModel_InvalidThreading) = 0,
		}}
		RT_ENUM! { enum MarshalingType: i32 {
			None (MarshalingType_None) = 1, Agile (MarshalingType_Agile) = 2, Standard (MarshalingType_Standard) = 3, InvalidMarshaling (MarshalingType_InvalidMarshaling) = 0,
		}}
		RT_ENUM! { enum DeprecationType: i32 {
			Deprecate (DeprecationType_Deprecate) = 0, Remove (DeprecationType_Remove) = 1,
		}}
} // Windows.Foundation.Metadata
pub mod diagnostics { // Windows.Foundation.Diagnostics
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_ENUM! { enum CausalityTraceLevel: i32 {
			Required (CausalityTraceLevel_Required) = 0, Important (CausalityTraceLevel_Important) = 1, Verbose (CausalityTraceLevel_Verbose) = 2,
		}}
		RT_ENUM! { enum CausalitySource: i32 {
			Application (CausalitySource_Application) = 0, Library (CausalitySource_Library) = 1, System (CausalitySource_System) = 2,
		}}
		RT_ENUM! { enum CausalityRelation: i32 {
			AssignDelegate (CausalityRelation_AssignDelegate) = 0, Join (CausalityRelation_Join) = 1, Choice (CausalityRelation_Choice) = 2, Cancel (CausalityRelation_Cancel) = 3, Error (CausalityRelation_Error) = 4,
		}}
		RT_ENUM! { enum CausalitySynchronousWork: i32 {
			CompletionNotification (CausalitySynchronousWork_CompletionNotification) = 0, ProgressNotification (CausalitySynchronousWork_ProgressNotification) = 1, Execution (CausalitySynchronousWork_Execution) = 2,
		}}
		DEFINE_IID!(IID_ITracingStatusChangedEventArgs, 1091270417, 65339, 18303, 156, 154, 210, 239, 218, 48, 45, 195);
		RT_INTERFACE!{interface ITracingStatusChangedEventArgs(ITracingStatusChangedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ITracingStatusChangedEventArgs] {
			fn get_Enabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_TraceLevel(&mut self, out: *mut ::rt::gen::windows::foundation::diagnostics::CausalityTraceLevel) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IAsyncCausalityTracerStatics, 1350896422, 9854, 17691, 168, 144, 171, 106, 55, 2, 69, 238);
		RT_INTERFACE!{interface IAsyncCausalityTracerStatics(IAsyncCausalityTracerStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IAsyncCausalityTracerStatics] {
			fn TraceOperationCreation(&mut self, traceLevel: ::rt::gen::windows::foundation::diagnostics::CausalityTraceLevel, source: ::rt::gen::windows::foundation::diagnostics::CausalitySource, platformId: ::w::GUID, operationId: u64, operationName: ::w::HSTRING, relatedContext: u64) -> ::w::HRESULT,
			fn TraceOperationCompletion(&mut self, traceLevel: ::rt::gen::windows::foundation::diagnostics::CausalityTraceLevel, source: ::rt::gen::windows::foundation::diagnostics::CausalitySource, platformId: ::w::GUID, operationId: u64, status: ::rt::gen::windows::foundation::AsyncStatus) -> ::w::HRESULT,
			fn TraceOperationRelation(&mut self, traceLevel: ::rt::gen::windows::foundation::diagnostics::CausalityTraceLevel, source: ::rt::gen::windows::foundation::diagnostics::CausalitySource, platformId: ::w::GUID, operationId: u64, relation: ::rt::gen::windows::foundation::diagnostics::CausalityRelation) -> ::w::HRESULT,
			fn TraceSynchronousWorkStart(&mut self, traceLevel: ::rt::gen::windows::foundation::diagnostics::CausalityTraceLevel, source: ::rt::gen::windows::foundation::diagnostics::CausalitySource, platformId: ::w::GUID, operationId: u64, work: ::rt::gen::windows::foundation::diagnostics::CausalitySynchronousWork) -> ::w::HRESULT,
			fn TraceSynchronousWorkCompletion(&mut self, traceLevel: ::rt::gen::windows::foundation::diagnostics::CausalityTraceLevel, source: ::rt::gen::windows::foundation::diagnostics::CausalitySource, work: ::rt::gen::windows::foundation::diagnostics::CausalitySynchronousWork) -> ::w::HRESULT,
			fn add_TracingStatusChanged(&mut self, handler: *mut ::rt::gen::windows::foundation::EventHandler<&::rt::gen::windows::foundation::diagnostics::TracingStatusChangedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_TracingStatusChanged(&mut self, cookie: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		RT_CLASS!(TracingStatusChangedEventArgs: ::rt::gen::windows::foundation::diagnostics::ITracingStatusChangedEventArgs);
		RT_ENUM! { enum ErrorOptions: u32 {
			None (ErrorOptions_None) = 0, SuppressExceptions (ErrorOptions_SuppressExceptions) = 1, ForceExceptions (ErrorOptions_ForceExceptions) = 2, UseSetErrorInfo (ErrorOptions_UseSetErrorInfo) = 4, SuppressSetErrorInfo (ErrorOptions_SuppressSetErrorInfo) = 8,
		}}
		DEFINE_IID!(IID_IErrorReportingSettings, 372676498, 45118, 19361, 139, 184, 210, 143, 74, 180, 210, 192);
		RT_INTERFACE!{interface IErrorReportingSettings(IErrorReportingSettingsVtbl): IInspectable(IInspectableVtbl) [IID_IErrorReportingSettings] {
			fn SetErrorOptions(&mut self, value: ::rt::gen::windows::foundation::diagnostics::ErrorOptions) -> ::w::HRESULT,
			fn GetErrorOptions(&mut self, out: *mut ::rt::gen::windows::foundation::diagnostics::ErrorOptions) -> ::w::HRESULT
		}}
		RT_CLASS!(RuntimeBrokerErrorSettings: ::rt::gen::windows::foundation::diagnostics::IErrorReportingSettings);
		DEFINE_IID!(IID_IErrorDetailsStatics, 3077584720, 2845, 18120, 170, 14, 75, 129, 120, 228, 252, 233);
		RT_INTERFACE!{interface IErrorDetailsStatics(IErrorDetailsStaticsVtbl): IInspectable(IInspectableVtbl) [IID_IErrorDetailsStatics] {
			fn CreateFromHResultAsync(&mut self, errorCode: i32, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::foundation::diagnostics::ErrorDetails>) -> ::w::HRESULT
		}}
		RT_CLASS!(ErrorDetails: ::rt::gen::windows::foundation::diagnostics::IErrorDetails);
		DEFINE_IID!(IID_IErrorDetails, 931969793, 11465, 17039, 140, 85, 44, 153, 13, 70, 62, 143);
		RT_INTERFACE!{interface IErrorDetails(IErrorDetailsVtbl): IInspectable(IInspectableVtbl) [IID_IErrorDetails] {
			fn get_Description(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_LongDescription(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_HelpUri(&mut self, out: *mut *mut ::rt::gen::windows::foundation::Uri) -> ::w::HRESULT
		}}
		RT_ENUM! { enum LoggingLevel: i32 {
			Verbose (LoggingLevel_Verbose) = 0, Information (LoggingLevel_Information) = 1, Warning (LoggingLevel_Warning) = 2, Error (LoggingLevel_Error) = 3, Critical (LoggingLevel_Critical) = 4,
		}}
		RT_ENUM! { enum LoggingOpcode: i32 {
			Info (LoggingOpcode_Info) = 0, Start (LoggingOpcode_Start) = 1, Stop (LoggingOpcode_Stop) = 2, Reply (LoggingOpcode_Reply) = 6, Resume (LoggingOpcode_Resume) = 7, Suspend (LoggingOpcode_Suspend) = 8, Send (LoggingOpcode_Send) = 9,
		}}
		RT_ENUM! { enum LoggingFieldFormat: i32 {
			Default (LoggingFieldFormat_Default) = 0, Hidden (LoggingFieldFormat_Hidden) = 1, String (LoggingFieldFormat_String) = 2, Boolean (LoggingFieldFormat_Boolean) = 3, Hexadecimal (LoggingFieldFormat_Hexadecimal) = 4, ProcessId (LoggingFieldFormat_ProcessId) = 5, ThreadId (LoggingFieldFormat_ThreadId) = 6, Port (LoggingFieldFormat_Port) = 7, Ipv4Address (LoggingFieldFormat_Ipv4Address) = 8, Ipv6Address (LoggingFieldFormat_Ipv6Address) = 9, SocketAddress (LoggingFieldFormat_SocketAddress) = 10, Xml (LoggingFieldFormat_Xml) = 11, Json (LoggingFieldFormat_Json) = 12, Win32Error (LoggingFieldFormat_Win32Error) = 13, NTStatus (LoggingFieldFormat_NTStatus) = 14, HResult (LoggingFieldFormat_HResult) = 15, FileTime (LoggingFieldFormat_FileTime) = 16, Signed (LoggingFieldFormat_Signed) = 17, Unsigned (LoggingFieldFormat_Unsigned) = 18,
		}}
		DEFINE_IID!(IID_ILoggingOptions, 2428270672, 402, 20317, 172, 38, 0, 106, 218, 202, 18, 216);
		RT_INTERFACE!{interface ILoggingOptions(ILoggingOptionsVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingOptions] {
			fn get_Keywords(&mut self, out: *mut i64) -> ::w::HRESULT,
			fn put_Keywords(&mut self, value: i64) -> ::w::HRESULT,
			fn get_Tags(&mut self, out: *mut i32) -> ::w::HRESULT,
			fn put_Tags(&mut self, value: i32) -> ::w::HRESULT,
			fn get_Task(&mut self, out: *mut i16) -> ::w::HRESULT,
			fn put_Task(&mut self, value: i16) -> ::w::HRESULT,
			fn get_Opcode(&mut self, out: *mut ::rt::gen::windows::foundation::diagnostics::LoggingOpcode) -> ::w::HRESULT,
			fn put_Opcode(&mut self, value: ::rt::gen::windows::foundation::diagnostics::LoggingOpcode) -> ::w::HRESULT,
			fn get_ActivityId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn put_ActivityId(&mut self, value: ::w::GUID) -> ::w::HRESULT,
			fn get_RelatedActivityId(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn put_RelatedActivityId(&mut self, value: ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingOptionsFactory, 3608397515, 39083, 17995, 159, 34, 163, 38, 132, 120, 54, 138);
		RT_INTERFACE!{interface ILoggingOptionsFactory(ILoggingOptionsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingOptionsFactory] {
			fn CreateWithKeywords(&mut self, keywords: i64, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingOptions) -> ::w::HRESULT
		}}
		RT_CLASS!(LoggingOptions: ::rt::gen::windows::foundation::diagnostics::ILoggingOptions);
		DEFINE_IID!(IID_ILoggingChannelOptions, 3286779903, 3771, 19027, 140, 84, 222, 194, 73, 38, 203, 44);
		RT_INTERFACE!{interface ILoggingChannelOptions(ILoggingChannelOptionsVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingChannelOptions] {
			fn get_Group(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT,
			fn put_Group(&mut self, value: ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingChannelOptionsFactory, 2838581722, 32687, 16785, 135, 85, 94, 134, 220, 101, 216, 150);
		RT_INTERFACE!{interface ILoggingChannelOptionsFactory(ILoggingChannelOptionsFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingChannelOptionsFactory] {
			fn Create(&mut self, group: ::w::GUID, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingChannelOptions) -> ::w::HRESULT
		}}
		RT_CLASS!(LoggingChannelOptions: ::rt::gen::windows::foundation::diagnostics::ILoggingChannelOptions);
		DEFINE_IID!(IID_ILoggingFields, 3623270319, 30253, 17785, 131, 189, 82, 194, 59, 195, 51, 188);
		RT_INTERFACE!{interface ILoggingFields(ILoggingFieldsVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingFields] {
			fn Clear(&mut self) -> ::w::HRESULT,
			fn BeginStruct(&mut self, name: ::w::HSTRING) -> ::w::HRESULT,
			fn BeginStructWithTags(&mut self, name: ::w::HSTRING, tags: i32) -> ::w::HRESULT,
			fn EndStruct(&mut self) -> ::w::HRESULT,
			fn AddEmpty(&mut self, name: ::w::HSTRING) -> ::w::HRESULT,
			fn AddEmptyWithFormat(&mut self, name: ::w::HSTRING, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddEmptyWithFormatAndTags(&mut self, name: ::w::HSTRING, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddUInt8(&mut self, name: ::w::HSTRING, value: u8) -> ::w::HRESULT,
			fn AddUInt8WithFormat(&mut self, name: ::w::HSTRING, value: u8, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddUInt8WithFormatAndTags(&mut self, name: ::w::HSTRING, value: u8, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddUInt8Array(&mut self, name: ::w::HSTRING, value: *mut u8) -> ::w::HRESULT,
			fn AddUInt8ArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut u8, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddUInt8ArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut u8, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddInt16(&mut self, name: ::w::HSTRING, value: i16) -> ::w::HRESULT,
			fn AddInt16WithFormat(&mut self, name: ::w::HSTRING, value: i16, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddInt16WithFormatAndTags(&mut self, name: ::w::HSTRING, value: i16, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddInt16Array(&mut self, name: ::w::HSTRING, value: *mut i16) -> ::w::HRESULT,
			fn AddInt16ArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut i16, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddInt16ArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut i16, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddUInt16(&mut self, name: ::w::HSTRING, value: u16) -> ::w::HRESULT,
			fn AddUInt16WithFormat(&mut self, name: ::w::HSTRING, value: u16, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddUInt16WithFormatAndTags(&mut self, name: ::w::HSTRING, value: u16, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddUInt16Array(&mut self, name: ::w::HSTRING, value: *mut u16) -> ::w::HRESULT,
			fn AddUInt16ArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut u16, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddUInt16ArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut u16, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddInt32(&mut self, name: ::w::HSTRING, value: i32) -> ::w::HRESULT,
			fn AddInt32WithFormat(&mut self, name: ::w::HSTRING, value: i32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddInt32WithFormatAndTags(&mut self, name: ::w::HSTRING, value: i32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddInt32Array(&mut self, name: ::w::HSTRING, value: *mut i32) -> ::w::HRESULT,
			fn AddInt32ArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut i32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddInt32ArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut i32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddUInt32(&mut self, name: ::w::HSTRING, value: u32) -> ::w::HRESULT,
			fn AddUInt32WithFormat(&mut self, name: ::w::HSTRING, value: u32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddUInt32WithFormatAndTags(&mut self, name: ::w::HSTRING, value: u32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddUInt32Array(&mut self, name: ::w::HSTRING, value: *mut u32) -> ::w::HRESULT,
			fn AddUInt32ArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut u32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddUInt32ArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut u32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddInt64(&mut self, name: ::w::HSTRING, value: i64) -> ::w::HRESULT,
			fn AddInt64WithFormat(&mut self, name: ::w::HSTRING, value: i64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddInt64WithFormatAndTags(&mut self, name: ::w::HSTRING, value: i64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddInt64Array(&mut self, name: ::w::HSTRING, value: *mut i64) -> ::w::HRESULT,
			fn AddInt64ArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut i64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddInt64ArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut i64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddUInt64(&mut self, name: ::w::HSTRING, value: u64) -> ::w::HRESULT,
			fn AddUInt64WithFormat(&mut self, name: ::w::HSTRING, value: u64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddUInt64WithFormatAndTags(&mut self, name: ::w::HSTRING, value: u64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddUInt64Array(&mut self, name: ::w::HSTRING, value: *mut u64) -> ::w::HRESULT,
			fn AddUInt64ArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut u64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddUInt64ArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut u64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddSingle(&mut self, name: ::w::HSTRING, value: f32) -> ::w::HRESULT,
			fn AddSingleWithFormat(&mut self, name: ::w::HSTRING, value: f32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddSingleWithFormatAndTags(&mut self, name: ::w::HSTRING, value: f32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddSingleArray(&mut self, name: ::w::HSTRING, value: *mut f32) -> ::w::HRESULT,
			fn AddSingleArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut f32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddSingleArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut f32, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddDouble(&mut self, name: ::w::HSTRING, value: f64) -> ::w::HRESULT,
			fn AddDoubleWithFormat(&mut self, name: ::w::HSTRING, value: f64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddDoubleWithFormatAndTags(&mut self, name: ::w::HSTRING, value: f64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddDoubleArray(&mut self, name: ::w::HSTRING, value: *mut f64) -> ::w::HRESULT,
			fn AddDoubleArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut f64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddDoubleArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut f64, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddChar16(&mut self, name: ::w::HSTRING, value: ::w::wchar_t) -> ::w::HRESULT,
			fn AddChar16WithFormat(&mut self, name: ::w::HSTRING, value: ::w::wchar_t, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddChar16WithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::w::wchar_t, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddChar16Array(&mut self, name: ::w::HSTRING, value: *mut ::w::wchar_t) -> ::w::HRESULT,
			fn AddChar16ArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::w::wchar_t, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddChar16ArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::w::wchar_t, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddBoolean(&mut self, name: ::w::HSTRING, value: ::w::BOOL) -> ::w::HRESULT,
			fn AddBooleanWithFormat(&mut self, name: ::w::HSTRING, value: ::w::BOOL, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddBooleanWithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::w::BOOL, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddBooleanArray(&mut self, name: ::w::HSTRING, value: *mut ::w::BOOL) -> ::w::HRESULT,
			fn AddBooleanArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::w::BOOL, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddBooleanArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::w::BOOL, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddString(&mut self, name: ::w::HSTRING, value: ::w::HSTRING) -> ::w::HRESULT,
			fn AddStringWithFormat(&mut self, name: ::w::HSTRING, value: ::w::HSTRING, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddStringWithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::w::HSTRING, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddStringArray(&mut self, name: ::w::HSTRING, value: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn AddStringArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::w::HSTRING, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddStringArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::w::HSTRING, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddGuid(&mut self, name: ::w::HSTRING, value: ::w::GUID) -> ::w::HRESULT,
			fn AddGuidWithFormat(&mut self, name: ::w::HSTRING, value: ::w::GUID, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddGuidWithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::w::GUID, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddGuidArray(&mut self, name: ::w::HSTRING, value: *mut ::w::GUID) -> ::w::HRESULT,
			fn AddGuidArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::w::GUID, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddGuidArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::w::GUID, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddDateTime(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn AddDateTimeWithFormat(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::DateTime, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddDateTimeWithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::DateTime, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddDateTimeArray(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::DateTime) -> ::w::HRESULT,
			fn AddDateTimeArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::DateTime, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddDateTimeArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::DateTime, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddTimeSpan(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn AddTimeSpanWithFormat(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::TimeSpan, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddTimeSpanWithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::TimeSpan, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddTimeSpanArray(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::TimeSpan) -> ::w::HRESULT,
			fn AddTimeSpanArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::TimeSpan, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddTimeSpanArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::TimeSpan, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddPoint(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Point) -> ::w::HRESULT,
			fn AddPointWithFormat(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Point, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddPointWithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Point, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddPointArray(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Point) -> ::w::HRESULT,
			fn AddPointArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Point, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddPointArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Point, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddSize(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn AddSizeWithFormat(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Size, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddSizeWithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Size, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddSizeArray(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Size) -> ::w::HRESULT,
			fn AddSizeArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Size, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddSizeArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Size, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddRect(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn AddRectWithFormat(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Rect, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddRectWithFormatAndTags(&mut self, name: ::w::HSTRING, value: ::rt::gen::windows::foundation::Rect, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT,
			fn AddRectArray(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Rect) -> ::w::HRESULT,
			fn AddRectArrayWithFormat(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Rect, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat) -> ::w::HRESULT,
			fn AddRectArrayWithFormatAndTags(&mut self, name: ::w::HSTRING, value: *mut ::rt::gen::windows::foundation::Rect, format: ::rt::gen::windows::foundation::diagnostics::LoggingFieldFormat, tags: i32) -> ::w::HRESULT
		}}
		RT_CLASS!(LoggingFields: ::rt::gen::windows::foundation::diagnostics::ILoggingFields);
		DEFINE_IID!(IID_ILoggingTarget, 1710320693, 58248, 20006, 177, 122, 245, 28, 211, 168, 57, 22);
		RT_INTERFACE!{interface ILoggingTarget(ILoggingTargetVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingTarget] {
			fn IsEnabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsEnabledWithLevel(&mut self, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn IsEnabledWithLevelAndKeywords(&mut self, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel, keywords: i64, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn LogEvent(&mut self, eventName: ::w::HSTRING) -> ::w::HRESULT,
			fn LogEventWithFields(&mut self, eventName: ::w::HSTRING, fields: *mut ::rt::gen::windows::foundation::diagnostics::LoggingFields) -> ::w::HRESULT,
			fn LogEventWithFieldsAndLevel(&mut self, eventName: ::w::HSTRING, fields: *mut ::rt::gen::windows::foundation::diagnostics::LoggingFields, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel) -> ::w::HRESULT,
			fn LogEventWithFieldsAndOptions(&mut self, eventName: ::w::HSTRING, fields: *mut ::rt::gen::windows::foundation::diagnostics::LoggingFields, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel, options: *mut ::rt::gen::windows::foundation::diagnostics::LoggingOptions) -> ::w::HRESULT,
			fn StartActivity(&mut self, startEventName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingActivity) -> ::w::HRESULT,
			fn StartActivityWithFields(&mut self, startEventName: ::w::HSTRING, fields: *mut ::rt::gen::windows::foundation::diagnostics::LoggingFields, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingActivity) -> ::w::HRESULT,
			fn StartActivityWithFieldsAndLevel(&mut self, startEventName: ::w::HSTRING, fields: *mut ::rt::gen::windows::foundation::diagnostics::LoggingFields, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingActivity) -> ::w::HRESULT,
			fn StartActivityWithFieldsAndOptions(&mut self, startEventName: ::w::HSTRING, fields: *mut ::rt::gen::windows::foundation::diagnostics::LoggingFields, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel, options: *mut ::rt::gen::windows::foundation::diagnostics::LoggingOptions, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingActivity) -> ::w::HRESULT
		}}
		RT_CLASS!(LoggingActivity: ::rt::gen::windows::foundation::diagnostics::ILoggingActivity);
		DEFINE_IID!(IID_ILoggingChannel, 3919905603, 4567, 20225, 181, 202, 207, 73, 82, 120, 192, 168);
		RT_INTERFACE!{interface ILoggingChannel(ILoggingChannelVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingChannel] {
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Enabled(&mut self, out: *mut ::w::BOOL) -> ::w::HRESULT,
			fn get_Level(&mut self, out: *mut ::rt::gen::windows::foundation::diagnostics::LoggingLevel) -> ::w::HRESULT,
			fn LogMessage(&mut self, eventString: ::w::HSTRING) -> ::w::HRESULT,
			fn LogMessageWithLevel(&mut self, eventString: ::w::HSTRING, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel) -> ::w::HRESULT,
			fn LogValuePair(&mut self, value1: ::w::HSTRING, value2: i32) -> ::w::HRESULT,
			fn LogValuePairWithLevel(&mut self, value1: ::w::HSTRING, value2: i32, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel) -> ::w::HRESULT,
			fn add_LoggingEnabled(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::foundation::diagnostics::ILoggingChannel, &IInspectable>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_LoggingEnabled(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingChannel2, 2672573683, 2988, 17829, 158, 51, 186, 243, 243, 162, 70, 165);
		RT_INTERFACE!{interface ILoggingChannel2(ILoggingChannel2Vtbl): IInspectable(IInspectableVtbl) [IID_ILoggingChannel2] {
			fn get_Id(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingChannelFactory, 1323064220, 44928, 19099, 176, 220, 57, 143, 154, 229, 32, 123);
		RT_INTERFACE!{interface ILoggingChannelFactory(ILoggingChannelFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingChannelFactory] {
			fn Create(&mut self, name: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingChannel) -> ::w::HRESULT
		}}
		RT_CLASS!(LoggingChannel: ::rt::gen::windows::foundation::diagnostics::ILoggingChannel);
		DEFINE_IID!(IID_ILoggingChannelFactory2, 1282340317, 15143, 19913, 153, 240, 41, 156, 110, 70, 3, 161);
		RT_INTERFACE!{interface ILoggingChannelFactory2(ILoggingChannelFactory2Vtbl): IInspectable(IInspectableVtbl) [IID_ILoggingChannelFactory2] {
			fn CreateWithOptions(&mut self, name: ::w::HSTRING, options: *mut ::rt::gen::windows::foundation::diagnostics::LoggingChannelOptions, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingChannel) -> ::w::HRESULT,
			fn CreateWithOptionsAndId(&mut self, name: ::w::HSTRING, options: *mut ::rt::gen::windows::foundation::diagnostics::LoggingChannelOptions, id: ::w::GUID, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingChannel) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingActivity, 3154323777, 46950, 19637, 152, 72, 151, 172, 107, 166, 214, 12);
		RT_INTERFACE!{interface ILoggingActivity(ILoggingActivityVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingActivity] {
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn get_Id(&mut self, out: *mut ::w::GUID) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingActivity2, 650287112, 25378, 17770, 175, 130, 128, 200, 100, 47, 23, 139);
		RT_INTERFACE!{interface ILoggingActivity2(ILoggingActivity2Vtbl): IInspectable(IInspectableVtbl) [IID_ILoggingActivity2] {
			fn get_Channel(&mut self, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingChannel) -> ::w::HRESULT,
			fn StopActivity(&mut self, stopEventName: ::w::HSTRING) -> ::w::HRESULT,
			fn StopActivityWithFields(&mut self, stopEventName: ::w::HSTRING, fields: *mut ::rt::gen::windows::foundation::diagnostics::LoggingFields) -> ::w::HRESULT,
			fn StopActivityWithFieldsAndOptions(&mut self, stopEventName: ::w::HSTRING, fields: *mut ::rt::gen::windows::foundation::diagnostics::LoggingFields, options: *mut ::rt::gen::windows::foundation::diagnostics::LoggingOptions) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingActivityFactory, 1798550659, 57610, 19544, 151, 213, 16, 251, 69, 16, 116, 251);
		RT_INTERFACE!{interface ILoggingActivityFactory(ILoggingActivityFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingActivityFactory] {
			fn CreateLoggingActivity(&mut self, activityName: ::w::HSTRING, loggingChannel: *mut ::rt::gen::windows::foundation::diagnostics::ILoggingChannel, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingActivity) -> ::w::HRESULT,
			fn CreateLoggingActivityWithLevel(&mut self, activityName: ::w::HSTRING, loggingChannel: *mut ::rt::gen::windows::foundation::diagnostics::ILoggingChannel, level: ::rt::gen::windows::foundation::diagnostics::LoggingLevel, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingActivity) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingSession, 1646392070, 37760, 19159, 186, 245, 65, 234, 147, 16, 215, 104);
		RT_INTERFACE!{interface ILoggingSession(ILoggingSessionVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingSession] {
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn SaveToFileAsync(&mut self, folder: *mut ::rt::gen::windows::storage::IStorageFolder, fileName: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT,
			fn AddLoggingChannel(&mut self, loggingChannel: *mut ::rt::gen::windows::foundation::diagnostics::ILoggingChannel) -> ::w::HRESULT,
			fn AddLoggingChannelWithLevel(&mut self, loggingChannel: *mut ::rt::gen::windows::foundation::diagnostics::ILoggingChannel, maxLevel: ::rt::gen::windows::foundation::diagnostics::LoggingLevel) -> ::w::HRESULT,
			fn RemoveLoggingChannel(&mut self, loggingChannel: *mut ::rt::gen::windows::foundation::diagnostics::ILoggingChannel) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_ILoggingSessionFactory, 1318289125, 22781, 17888, 140, 47, 161, 50, 239, 249, 92, 30);
		RT_INTERFACE!{interface ILoggingSessionFactory(ILoggingSessionFactoryVtbl): IInspectable(IInspectableVtbl) [IID_ILoggingSessionFactory] {
			fn Create(&mut self, name: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::LoggingSession) -> ::w::HRESULT
		}}
		RT_CLASS!(LoggingSession: ::rt::gen::windows::foundation::diagnostics::ILoggingSession);
		DEFINE_IID!(IID_ILogFileGeneratedEventArgs, 647927663, 3384, 19482, 181, 63, 179, 149, 216, 129, 223, 132);
		RT_INTERFACE!{interface ILogFileGeneratedEventArgs(ILogFileGeneratedEventArgsVtbl): IInspectable(IInspectableVtbl) [IID_ILogFileGeneratedEventArgs] {
			fn get_File(&mut self, out: *mut *mut ::rt::gen::windows::storage::StorageFile) -> ::w::HRESULT
		}}
		RT_CLASS!(LogFileGeneratedEventArgs: ::rt::gen::windows::foundation::diagnostics::ILogFileGeneratedEventArgs);
		DEFINE_IID!(IID_IFileLoggingSession, 617038358, 65234, 16460, 137, 95, 31, 150, 153, 203, 2, 247);
		RT_INTERFACE!{interface IFileLoggingSession(IFileLoggingSessionVtbl): IInspectable(IInspectableVtbl) [IID_IFileLoggingSession] {
			fn get_Name(&mut self, out: *mut ::w::HSTRING) -> ::w::HRESULT,
			fn AddLoggingChannel(&mut self, loggingChannel: *mut ::rt::gen::windows::foundation::diagnostics::ILoggingChannel) -> ::w::HRESULT,
			fn AddLoggingChannelWithLevel(&mut self, loggingChannel: *mut ::rt::gen::windows::foundation::diagnostics::ILoggingChannel, maxLevel: ::rt::gen::windows::foundation::diagnostics::LoggingLevel) -> ::w::HRESULT,
			fn RemoveLoggingChannel(&mut self, loggingChannel: *mut ::rt::gen::windows::foundation::diagnostics::ILoggingChannel) -> ::w::HRESULT,
			fn CloseAndSaveToFileAsync(&mut self, out: *mut *mut ::rt::gen::windows::foundation::IAsyncOperation<&::rt::gen::windows::storage::StorageFile>) -> ::w::HRESULT,
			fn add_LogFileGenerated(&mut self, handler: *mut ::rt::gen::windows::foundation::TypedEventHandler<&::rt::gen::windows::foundation::diagnostics::IFileLoggingSession, &::rt::gen::windows::foundation::diagnostics::LogFileGeneratedEventArgs>, out: *mut ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT,
			fn remove_LogFileGenerated(&mut self, token: ::rt::gen::windows::foundation::EventRegistrationToken) -> ::w::HRESULT
		}}
		DEFINE_IID!(IID_IFileLoggingSessionFactory, 4003499470, 33863, 19882, 145, 51, 18, 235, 70, 246, 151, 212);
		RT_INTERFACE!{interface IFileLoggingSessionFactory(IFileLoggingSessionFactoryVtbl): IInspectable(IInspectableVtbl) [IID_IFileLoggingSessionFactory] {
			fn Create(&mut self, name: ::w::HSTRING, out: *mut *mut ::rt::gen::windows::foundation::diagnostics::FileLoggingSession) -> ::w::HRESULT
		}}
		RT_CLASS!(FileLoggingSession: ::rt::gen::windows::foundation::diagnostics::IFileLoggingSession);
} // Windows.Foundation.Diagnostics
pub mod numerics { // Windows.Foundation.Numerics
use ::{ComInterface, HString, HStringRef, ComPtr, ComIid, IUnknown};
use ::rt::{RtInterface, RtType, RtValueType, IInspectable}; use ::rt::handler::IntoInterface;
		RT_STRUCT! { struct Vector2 {
			X: f32, Y: f32,
		}}
		RT_STRUCT! { struct Vector3 {
			X: f32, Y: f32, Z: f32,
		}}
		RT_STRUCT! { struct Vector4 {
			X: f32, Y: f32, Z: f32, W: f32,
		}}
		RT_STRUCT! { struct Matrix3x2 {
			M11: f32, M12: f32, M21: f32, M22: f32, M31: f32, M32: f32,
		}}
		RT_STRUCT! { struct Matrix4x4 {
			M11: f32, M12: f32, M13: f32, M14: f32, M21: f32, M22: f32, M23: f32, M24: f32, M31: f32, M32: f32, M33: f32, M34: f32, M41: f32, M42: f32, M43: f32, M44: f32,
		}}
		RT_STRUCT! { struct Plane {
			Normal: ::rt::gen::windows::foundation::numerics::Vector3, D: f32,
		}}
		RT_STRUCT! { struct Quaternion {
			X: f32, Y: f32, Z: f32, W: f32,
		}}
} // Windows.Foundation.Numerics
} // Windows.Foundation
} // Windows
