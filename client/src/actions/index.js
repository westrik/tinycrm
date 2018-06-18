import { CALL_API, Schemas } from '../middleware/api'

export const USER_REQUEST = 'USER_REQUEST'
export const USER_SUCCESS = 'USER_SUCCESS'
export const USER_FAILURE = 'USER_FAILURE'

// Fetches a single user from TinyCRM API.
// Relies on the custom API middleware defined in ../middleware/api.js.
const fetchUser = userId => ({
  [CALL_API]: {
    types: [ USER_REQUEST, USER_SUCCESS, USER_FAILURE ],
    endpoint: `users/${userId}`,
    schema: Schemas.USER
  }
})

// Fetches a single user from TinyCRM API unless it is cached.
// Relies on Redux Thunk middleware.
export const loadUser = (id, requiredFields = []) => (dispatch, getState) => {
  const user = getState().entities.users[id]
  if (user && requiredFields.every(key => user.hasOwnProperty(key))) {
    return null
  }

  return dispatch(fetchUser(id))
}

export const RESET_ERROR_MESSAGE = 'RESET_ERROR_MESSAGE'

// Resets the currently visible error message.
export const resetErrorMessage = () => ({
    type: RESET_ERROR_MESSAGE
})
