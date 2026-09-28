# 049_DeviceConfig.R
# ::rtemis::
# 2026- EDG rtemis.org

# %% DeviceConfig ----
#' DeviceConfig
#'
#' @description
#' Abstract family for the compute device an execution config requests. Each
#' device kind is a variant carrying only its own settings, so GPU ids can be
#' written only beside a device that has them.
#'
#' @field type Character: Device kind (computed constant, overridden per
#'   subclass).
#'
#' @author EDG
#' @keywords internal
#' @noRd
DeviceConfig <- schema_class(
  name = "DeviceConfig",
  package = "rtemis",
  abstract = TRUE,
  properties = list(
    type = class_character
  ),
  publication = SchemaPublication(
    role = "family",
    slug = "device",
    title = "rtemis DeviceConfig",
    description = "Language-independent config for the compute device a run requests: a device type plus that type's own settings. An algorithm that cannot use the requested device runs on the CPU.",
    discriminator = "type",
    discriminator_description = "Device type.",
    order = 11L
  )
) # /rtemis::DeviceConfig


# %% serializable_props.DeviceConfig ----
# `type` plus the device's settings as siblings -- the shape every family
# declares.
method(serializable_props, DeviceConfig) <- function(x) {
  dispatched_props(x, DeviceConfig, "type")
} # /rtemis::serializable_props.DeviceConfig


# %% CPUDeviceConfig ----
#' @keywords internal
#' @noRd
CPUDeviceConfig <- schema_class(
  name = "CPUDeviceConfig",
  parent = DeviceConfig,
  properties = list(
    type = prop_algorithm("cpu")
  ),
  publication = SchemaPublication(
    role = "leaf",
    description = "The CPU.",
    order = 1L
  )
) # /rtemis::CPUDeviceConfig


# %% CUDADeviceConfig ----
#' @keywords internal
#' @noRd
CUDADeviceConfig <- schema_class(
  name = "CUDADeviceConfig",
  parent = DeviceConfig,
  properties = list(
    type = prop_algorithm("cuda"),
    ids = prop_integer(
      NULL,
      min = 0L,
      nullable = TRUE,
      vector = TRUE,
      min_items = 1L,
      unique_items = TRUE,
      description = "Zero-based indices of the NVIDIA GPUs to use. Unset uses the first visible GPU."
    )
  ),
  publication = SchemaPublication(
    role = "leaf",
    description = "An NVIDIA GPU through CUDA.",
    order = 2L
  )
) # /rtemis::CUDADeviceConfig


# %% setup_CUDA ----
#' Setup an NVIDIA GPU as the compute device
#'
#' Request an NVIDIA GPU through CUDA as the compute device of an execution
#' config, naming which GPUs to use, e.g.
#' `setup_FutureExecution(device = setup_CUDA(ids = 1L))`. Without ids,
#' `device = "cuda"` is the same request. Algorithms that run on CUDA are the torch-backed ones
#' (MLP, TabNet) and the LightGBM family built with CUDA support; every other
#' algorithm runs on the CPU.
#'
#' @param ids Optional Integer [0, Inf) vector: Zero-based indices of the GPUs to
#' use. `NULL` uses the first visible GPU. A run uses the first index listed.
#'
#' @return `CUDADeviceConfig` object.
#'
#' @author EDG
#' @export
#' @examples
#' setup_CUDA()
#' setup_CUDA(ids = 1L)
setup_CUDA <- function(ids = NULL) {
  apply_setup_defaults(CUDADeviceConfig)
  if (!is.null(ids)) {
    ids <- clean_int(ids)
  }
  CUDADeviceConfig(ids = ids)
} # /rtemis::setup_CUDA


# %% MPSDeviceConfig ----
#' @keywords internal
#' @noRd
MPSDeviceConfig <- schema_class(
  name = "MPSDeviceConfig",
  parent = DeviceConfig,
  properties = list(
    type = prop_algorithm("mps")
  ),
  publication = SchemaPublication(
    role = "leaf",
    description = "The Apple silicon GPU through Metal Performance Shaders.",
    order = 3L
  )
) # /rtemis::MPSDeviceConfig


# %% OpenCLDeviceConfig ----
#' @keywords internal
#' @noRd
OpenCLDeviceConfig <- schema_class(
  name = "OpenCLDeviceConfig",
  parent = DeviceConfig,
  properties = list(
    type = prop_algorithm("opencl")
  ),
  publication = SchemaPublication(
    role = "leaf",
    description = "A GPU through OpenCL, used by a LightGBM build with GPU support.",
    order = 4L
  )
) # /rtemis::OpenCLDeviceConfig


# %% DEVICE_BUILDERS ----
# The device family, keyed by the `type` value each variant declares. Only CUDA
# has settings, so only it has a `setup_*` function; the others are written as
# their type name and built here.
DEVICE_BUILDERS <- list(
  cpu = function() CPUDeviceConfig(),
  cuda = function(ids = NULL) setup_CUDA(ids = ids),
  mps = function() MPSDeviceConfig(),
  opencl = function() OpenCLDeviceConfig()
)


# %% .list_to_DeviceConfig ----
#' Convert a list to a DeviceConfig object
#'
#' @param x Named list with a `type` element plus the device's settings as its
#'   siblings, e.g. `list(type = "cuda", ids = list(0L, 1L))`.
#'
#' @return A `DeviceConfig` object (a device-specific subclass).
#'
#' @author EDG
#' @keywords internal
#' @noRd
.list_to_DeviceConfig <- function(x) {
  if (S7_inherits(x, DeviceConfig)) {
    return(x)
  }
  type <- x[["type"]]
  if (is.null(type) || !type %in% names(DEVICE_BUILDERS)) {
    rtemis.core::abort(
      "A device needs a `type`, one of: ",
      paste0("\"", names(DEVICE_BUILDERS), "\"", collapse = ", "),
      ".",
      class = c("rtemis_value_error", "rtemis_input_error")
    )
  }
  setup_fn <- DEVICE_BUILDERS[[type]]
  params <- .drop_meta_keys(x)
  params[["type"]] <- NULL
  check_wire_keys(params, names(formals(setup_fn)), paste(type, "device"))
  # A JSON array parsed without simplification arrives as a list of scalars.
  if (!is.null(params[["ids"]])) {
    params[["ids"]] <- unlist(params[["ids"]], use.names = FALSE)
  }
  do.call(setup_fn, Filter(Negate(is.null), params))
} # /rtemis::.list_to_DeviceConfig


# %% as_device_config ----
#' Normalize a device argument
#'
#' Setup functions accept a device as a type name such as `"mps"`, a
#' `setup_CUDA()` object, or a wire list; all three become the object.
#'
#' @param device Optional `DeviceConfig`, Character, or list.
#'
#' @return `DeviceConfig` object, or NULL.
#'
#' @author EDG
#' @keywords internal
#' @noRd
as_device_config <- function(device) {
  if (is.null(device) || S7_inherits(device, DeviceConfig)) {
    return(device)
  }
  if (is.character(device) && length(device) == 1L) {
    return(.list_to_DeviceConfig(list(type = device)))
  }
  if (is.list(device)) {
    return(.list_to_DeviceConfig(device))
  }
  rtemis.core::abort(
    "`device` must be a device type, one of \"cpu\", \"cuda\", \"mps\", ",
    "\"opencl\", or a `setup_CUDA()` object.",
    class = c("rtemis_type_error", "rtemis_input_error")
  )
} # /rtemis::as_device_config
