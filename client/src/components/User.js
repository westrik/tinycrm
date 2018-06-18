import React from 'react'
import PropTypes from 'prop-types'
import { Link } from 'react-router-dom'

const User = ({ user }) => {
  console.log(user)
  const { id, email, name } = user

  return (
    <div className="User">
      <Link to={`/${id}`}>
        <h3>
          {email} {<span>({name})</span>}
        </h3>
      </Link>
    </div>
  )
}

User.propTypes = {
  id: PropTypes.string.isRequired,
  email: PropTypes.string.isRequired,
  name: PropTypes.string.isRequired
}

export default User
