import React from 'react'
import PropTypes from 'prop-types'
import { Link } from 'react-router-dom'

const User = ({ user }) => {
  const { login, name, email } = user

  return (
    <div className="User">
      <Link to={`/${login}`}>
        <h3>
          <span>{name} ({login}, {email})</span>
        </h3>
      </Link>
    </div>
  )
}

User.propTypes = {
  user: PropTypes.shape({
    login: PropTypes.string.isRequired,
    name: PropTypes.string,
    email: PropTypes.string
  }).isRequired
}

export default User
